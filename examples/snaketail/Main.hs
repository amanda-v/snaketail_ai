{-
Snaketail is built with haskell and sdl
-}

{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module Main where

import Prelude hiding (head, init, tail)
import Data.Word (Word32)
import Data.List.NonEmpty (NonEmpty((:|)), head, init, tail, toList)
import Foreign.C.Types (CInt)
import Control.Monad (when, unless)
import Control.Concurrent (threadDelay)
import SDL.Vect (Point(P), V2(..), V4(..))
import SDL (($=))
import System.Random (StdGen, getStdGen, mkStdGen, randomR)
import qualified SDL
import Data.Maybe (isJust, fromJust)

import Smarties

-------------------------
-- Types and Constants --
-------------------------

data GameState
  = GameState
  { sSnake :: NonEmpty (V2 CInt) -- we model the snake as a non-empty list of blocks
  , sDirection :: (Maybe Direction)
  , sStatus :: SnakeStatus
  , sFood :: Maybe (V2 CInt)
  , sMoveTimer :: Int -- we use timers to control when stuff should happen
  , sFoodTimer :: Int
  , sRandomGen :: StdGen -- this is used to generate new food at psuedo random locations
  }
  deriving Show

data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving Show

data SnakeStatus
  = Alive
  | Dead
  | CollidedWithTail
  | CollidedWithWall
  deriving (Show, Eq)

data MyEvents
  = MyEvents
  { eQuit :: Bool
  , eArrowUp :: Bool
  , eArrowDown :: Bool
  , eArrowLeft :: Bool
  , eArrowRight :: Bool
  }
  deriving Show

---------------
-- Main Loop --
---------------

type TreeStateType = (NonEmpty (V2 CInt), Maybe (V2 CInt), (Maybe Direction))
type ActionType = Direction

-- calculateDirection snake snaketail fruit =
--   if getX fruit < getX snake
--     then DirLeft
--     else if getX fruit > getX snake
--       then DirRight
--       else if getY fruit < getY snake
--         then DirUp
--         else DirDown

-- Avoid going backwards
calculateDirection' snake (snaketail) fruit
  | getX fruit < getX snake && getX snake <= getX ( snaketail) = DirLeft
  | getX fruit < getX snake && getX snake > getX ( snaketail) = DirUp
  | getX fruit > getX snake && getX snake >= getX ( snaketail) = DirRight
  | getX fruit > getX snake && getX snake < getX ( snaketail) = DirUp
  | getY fruit < getY snake && getY snake <= getY ( snaketail) = DirUp
  | getY fruit < getY snake && getY snake > getY ( snaketail) = DirLeft
  | getY fruit > getY snake && getY snake >= getY ( snaketail) = DirDown
  | otherwise = DirLeft 
-- calculateDirection''
getTail snaketail
  | length snaketail > 1 = snaketail !! 1
  | otherwise = snaketail !! 0
  

ifFruitExists :: NodeSequence g TreeStateType ActionType ()
ifFruitExists = do
  (_, fruit, _) <- getPerception
  condition $ isJust fruit

getDirectionOfFruit :: NodeSequence g TreeStateType ActionType (Direction)
getDirectionOfFruit = do
  (us, fruit, _) <- getPerception
  return $ calculateDirection' (head us) (getTail (toList us)) (fromJust fruit)

move :: Direction -> NodeSequence g TreeStateType ActionType ()
move dir = fromAction $ SimpleAction (\_ -> dir)

snakeTree :: NodeSequence g TreeStateType ActionType ()
snakeTree = do
  ifFruitExists -- sequence
  dir <- getDirectionOfFruit
  move dir

-- snakeTreeNoSequence :: NodeSequence g TreeStateType ActionType ()
-- snakeTreeNoSequence = do
--  -- b <- ifFruitExistsBool
--  -- if b == True
--   -- then do
--   dir <- getDirectionOfFruit
--   move dir
--   -- else
--  --   return ()

runSnakeTree :: GameState -> GameState
runSnakeTree gameState = r where
  perception = extractPerception gameState
  (_, _, _, output) = execNodeSequence snakeTree () perception
  r = if null output then gameState else gameState { sDirection = Just $ last output }

-- gets the TreeStateType perception out of the game state.
extractPerception gameState = ((sSnake gameState), (sFood gameState), (sDirection gameState))

run :: IO ()
run = do
  SDL.initialize [SDL.InitVideo]

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SnakeTail"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
         { SDL.rendererType = SDL.AcceleratedRenderer
         , SDL.rendererTargetTexture = False
         }

  SDL.rendererDrawColor renderer $= V4 0 0 0 0 -- black background

  randomGen <- getStdGen
  loop renderer initGameState { sRandomGen = randomGen }

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

loop :: SDL.Renderer -> GameState -> IO ()
loop renderer state = do
  -- measure ticks at the start
  start <- SDL.ticks

  -- the pattern
  events <- fetchEvents
  -- putStrLn $ show $ events
  let state' = update events state
  let state'' = runSnakeTree state'
  render renderer state''

  -- measure ticks at the end and regulate FPS
  end <- SDL.ticks
  regulateFPS 60 start end

  -- decide whether to continue or not
  unless (eQuit events) (loop renderer state'')

-- | Will wait until ticks pass
regulateFPS :: Word32 -> Word32 -> Word32 -> IO ()
regulateFPS fps start end
  | fps == 0 = pure ()
  | otherwise = do
    let
      ticksPerFrame = 1000 `div` fps
      interval = end - start
      gap = ticksPerFrame - interval 
      delayFor
        | gap < ticksPerFrame =
          fromIntegral $ max 0 gap
        | otherwise =
          fromIntegral ticksPerFrame
    threadDelay $ delayFor * 1000 -- threadDelay works in microseconds
 
initGameState :: GameState
initGameState = GameState
  { sSnake = V2 (blockSize * 7) (blockSize * 7) :| []
  , sDirection = Just DirRight
  , sStatus = Alive
  , sFood = Just $ V2 (23 * blockSize) (14 * blockSize)
  , sMoveTimer = 13 -- the units are frames
  , sFoodTimer = 360 -- the units are frames
  , sRandomGen = mkStdGen 17 -- filled in to silence compiler warning, but should be replaced with a random one
  }

getX :: V2 CInt -> CInt
getX (V2 locX _) = locX

getY :: V2 CInt -> CInt
getY (V2 _ locY) = locY

snakeBodyBlockSize :: V2 CInt
snakeBodyBlockSize = V2 blockSize blockSize

blockSize :: CInt
blockSize = 24

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (1024, 786)


---------------------
-- Game Processing --
---------------------

------------
-- Events --
------------

-- | Will return a record with the relevant event states
fetchEvents :: IO MyEvents
fetchEvents = do
  events <- SDL.pollEvents
  isKeyPressed <- SDL.getKeyboardState
  pure $ MyEvents
    { eQuit = elem SDL.QuitEvent $ map SDL.eventPayload events
    , eArrowUp = isKeyPressed SDL.ScancodeUp
    , eArrowDown = isKeyPressed SDL.ScancodeDown
    , eArrowLeft = isKeyPressed SDL.ScancodeLeft
    , eArrowRight = isKeyPressed SDL.ScancodeRight
    }

------------
-- Update --
------------

-- | Will update the game each frame
update :: MyEvents -> GameState -> GameState
update events state
  | sStatus state == Dead = state
  | otherwise =
    collide . updateFood . moveAndEat . changeDir events $ state

changeDir :: MyEvents -> GameState -> GameState
changeDir events state
  | eArrowUp    events = state { sDirection = Just DirUp }
  | eArrowDown  events = state { sDirection = Just DirDown }
  | eArrowLeft  events = state { sDirection = Just DirLeft }
  | eArrowRight events = state { sDirection = Just DirRight }
  | otherwise          = state

moveAndEat :: GameState -> GameState
moveAndEat state
  | sMoveTimer state == 0 =
    state
      { sMoveTimer = sMoveTimer initGameState - min (sMoveTimer initGameState - 2) (length (sSnake state))
      , sSnake =
        newBlock (sDirection state) (head $ sSnake state)
        :| (if ate state then toList else init) (sSnake state)
      , sFood =
        if ate state
          then Nothing
          else sFood state
      , sFoodTimer =
        if ate state
          then 60
          else sFoodTimer state - 1
      }
  | otherwise = state
    { sMoveTimer = sMoveTimer state - 1
    , sFoodTimer = sFoodTimer state - 1
    }

ate :: GameState -> Bool
ate state = Just (head $ sSnake state) == sFood state

newBlock :: Maybe Direction -> V2 CInt -> V2 CInt
newBlock dir (V2 locX locY) = case dir of
  Just DirUp -> V2 locX (locY - getY snakeBodyBlockSize)
  Just DirDown -> V2 locX (locY + getY snakeBodyBlockSize)
  Just DirLeft -> V2 (locX - getX snakeBodyBlockSize) locY
  Just DirRight -> V2 (locX + getX snakeBodyBlockSize) locY
  Nothing -> V2 locX locY
  
updateFood :: GameState -> GameState
updateFood state
  | sFoodTimer state == 0 = 
    let
      ((* blockSize) -> x, stdGen')  = randomR (4, div screenWidth  blockSize - 4) (sRandomGen state)
      ((* blockSize) -> y, stdGen'') = randomR (4, div screenHeight blockSize - 4) stdGen'
    in state
      { sFoodTimer = sFoodTimer initGameState
      , sFood = maybe (Just $ V2 x y) (const Nothing) $ sFood state 
      , sRandomGen = stdGen''
      }
  | otherwise =
    state

collide :: GameState -> GameState
collide state
  | sStatus state /= Alive = 
    state
    { sStatus = Dead
    , sDirection = Nothing
    }
  | any (head (sSnake state) ==) (tail $ sSnake state) =
    state
    { sStatus = CollidedWithTail
    , sDirection = Nothing
    }
  | getX (head $ sSnake state) < 0
    || getX (head $ sSnake state) >= screenWidth
    || getY (head $ sSnake state) < 0
    || getY (head $ sSnake state) >= screenHeight
    = state
    { sStatus = CollidedWithWall
    , sDirection = Nothing
    }
  | otherwise = state

---------------
-- Rendering --
---------------

-- | Will render the game on screen
render :: SDL.Renderer -> GameState -> IO ()
render renderer state = do
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  SDL.clear renderer

  let
    drawBlock location =
      SDL.fillRect renderer $
        Just $ SDL.Rectangle (P location) snakeBodyBlockSize

  SDL.rendererDrawColor renderer $= V4 0 maxBound 0 maxBound 
  mapM_ drawBlock $ sFood state

  SDL.rendererDrawColor renderer $= V4 0 0 0 0
  mapM_ drawBlock $ sSnake state

  when (sStatus state == CollidedWithTail) $
    putStrLn "The snake collided with it's tail :("

  when (sStatus state == CollidedWithWall) $
    putStrLn "The snake collided with the wall :("

  SDL.present renderer

main :: IO ()
main = Main.run
