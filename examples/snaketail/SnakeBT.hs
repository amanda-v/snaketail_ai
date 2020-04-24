module SnakeBT where
import Smarties
 
-- Data type Snake.hs
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
 
 
{-
   a: monad output type, typically used for computing utility
   g: random generator
   p: perception type
   Status: Status of executing NodeSequence, either SUCCESS or FAIL
   o: output type
 
   data NodeSequenceT g p o m a =  NodeSequence { runNodes :: g -> p -> (a, g, p, Status, [o]) }
 
   type NodeSequence g p o a = NodeSequence T g p o Identity a
-}
 

type TreeStateType = (NonEmpty (V2 CInt), Maybe (V2 CInt), (Maybe Direction))
type ActionType = Direction
 
 
getDirectionOfFruit :: NodeSequence g TreeStateType ActionType Direction
getDirectionOfFruit = do
	(us, fruit, _) <- getPerception
	Return $ calculateDirection us fruit
 
ifFruitExists :: NodeSequence g TreeStateType ActionType ()
ifFruitExists = do
(_, fruit, _) 
	Condition $ isJust fruit
--Return $ isJustFruit
 
ifFruitExistsBool :: NodeSequence g TreeStateType ActionType ()
ifFruitExistsBool = do
(_, fruit, _) 
	Return $ isJustFruit
 
Move :: Direction -> NodeSequence g TreeStateType ActionType ()
Move dir = fromAction $ SimpleAction (\_ -> dir)
 
 
snakeTree :: NodeSequence g TreeStateType ActionType ()
snakeTree = do
	ifFruitExists
	Dir <- getDirectionOfFruit
	Move dir
 
snakeTreeNoSequence :: NodeSequence g TreeStateType ActionType ()
snakeTreeNoSequence = do
	B <- ifFruitExistsBool
	If b then do
	Dir <- getDirectionOfFruit
	Move dir
Else
	Return ()

runSnakeTree :: GameState -> GameState
runSnakeTree gameState = r where
	Perception = extractData gameState
	output = execNodeSequence snakeTree () perception 
	r = if nil output then gameState else gameState { sDirection = last output }


