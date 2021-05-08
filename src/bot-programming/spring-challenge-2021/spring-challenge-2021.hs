-- https://www.codingame.com/contests/spring-challenge-2021
import Control.Monad
import System.IO
import Data.List

debug = hPutStrLn stderr

data Richness = Low | Medium | High
  deriving Show

toRichness :: Int -> Richness
toRichness 1 = Low
toRichness 2 = Medium
toRichness 3 = High

data Cell = Cell { cellIndex :: Int
                 , cellRichness :: Richness
                 , cellNeighbours :: [Int]
} deriving Show

type Forest = [Cell]

newtype TreeSize = TreeSize Int
  deriving Show

seed = TreeSize 0

data Tree = Tree { treeCellIndex :: Int
                 , treeSize :: TreeSize
                 , treeIsMine :: Bool
                 , treeIsDormant :: Bool
} deriving Show

data Action = WAIT | COMPLETE Int
  deriving Show

toAction :: [String] -> Action
toAction ("WAIT" : _) =  WAIT
toAction ("COMPLETE": xs) = COMPLETE (read (head xs) :: Int)

isComplete :: Action -> Bool
isComplete c@(COMPLETE _) = True
isComplete _ = False

data Turn = Turn { tDay :: Int
                 , tNutrients :: Int
                 , tMySun :: Int
                 , tMyScore :: Int
                 , tOppSun :: Int
                 , tOppScore :: Int
                 , tOppIsWaiting :: Bool
                 , tTrees :: [Tree]
                 , tPossibleActions :: [Action]
} deriving Show

pickComplete :: [Action] -> Maybe Action
pickComplete = find isComplete

chooseAction :: Maybe Action -> Action
chooseAction (Just a) = a
chooseAction Nothing = WAIT

doMagic :: IO ()
doMagic = do
    turn <- readTurn

    debug (show turn)

    let action = chooseAction $ pickComplete (tPossibleActions turn)

    -- hPutStrLn stderr "Debug messages..."

    -- GROW cellIdx | SEED sourceIdx targetIdx | COMPLETE cellIdx | WAIT <message>

    putStrLn (show action)

---------------------------------------------------------------------------------------------------
---------------------------------------------- MAIN -----------------------------------------------
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  -- Auto-generated code below aims at helping you parse
  -- the standard input according to the problem statement.

  input_line <- getLine
  cells <- readCells (read input_line :: Int)

  -- game loop
  forever doMagic

readTurn = do
  input_line <- getLine
  let day = read input_line :: Int -- the game lasts 24 days: 0-23
  input_line <- getLine
  let nutrients = read input_line :: Int -- the base score you gain from the next COMPLETE action
  input_line <- getLine
  let input = words input_line
  let sun = read (input !! 0) :: Int -- your sun points
  let score = read (input !! 1) :: Int -- your current score
  input_line <- getLine
  let input = words input_line
  let oppsun = read (input !! 0) :: Int -- opponent's sun points
  let oppscore = read (input !! 1) :: Int -- opponent's score
  let oppiswaiting = read (input !! 2) == 1 :: Bool -- whether your opponent is asleep until the next day
  input_line <- getLine
  let numberoftrees = read input_line :: Int -- the current amount of trees
  trees <- readTrees numberoftrees
  input_line <- getLine
  let numberofpossibleactions = read input_line :: Int -- all legal actions
  actions <- readActions numberofpossibleactions
  return (Turn day nutrients sun score oppsun oppscore oppiswaiting trees actions)


treeFromString input_line = Tree cellindex (TreeSize size) ismine isdormant
  where
    input = words input_line
    cellindex = read (input !! 0) :: Int -- location of this tree
    size = read (input !! 1) :: Int -- size of this tree: 0-3
    ismine = read (input !! 2) == 1 :: Bool -- 1 if this is your tree
    isdormant = read (input !! 3) == 1 :: Bool -- 1 if this tree is dormant

readTrees n = replicateM n $ do
  treeFromString <$> getLine

actionFromString :: String -> Action
actionFromString s = toAction xs
  where xs = words s

readActions n = replicateM n $ do
  actionFromString <$> getLine

cellFromString input_line = Cell {cellIndex=index, cellRichness=richness, cellNeighbours=neighbours}
  where
    input = words input_line
    index = read (input !! 0) :: Int -- 0 is the center cell, the next cells spiral outwards
    richness = toRichness (read (input !! 1) :: Int) -- 0 if the cell is unusable, 1-3 for usable cells
    neighbours = [read (input !! i) :: Int | i <- [2..7]] -- the index of the neighbouring cell for each direction

readCells n = replicateM n $ do
    cellFromString <$> getLine