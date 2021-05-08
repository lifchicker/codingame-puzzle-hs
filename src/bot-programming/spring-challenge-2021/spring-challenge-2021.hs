-- https://www.codingame.com/contests/spring-challenge-2021
import Control.Monad
import System.IO
import Data.List
import Data.Maybe (isJust, fromJust, listToMaybe)

debug = hPutStrLn stderr

type Richness = Int

data Cell = Cell { _index :: Int
                 , _richness :: Richness
                 , _neighbours :: [Int]
} deriving Show

type Forest = [Cell]

type TreeSize = Int

seed = 0 :: TreeSize

bigger :: TreeSize -> TreeSize
bigger = succ

data Tree = Tree { _cellIndex :: Int
                 , _treeSize :: TreeSize
                 , _isMine :: Bool
                 , _isDormant :: Bool
} deriving Show

data Action = WAIT
            | COMPLETE Int
            | GROW Int
            | SEED Int Int
  deriving Show

toAction :: [String] -> Action
toAction ("WAIT" : _) =  WAIT
toAction ("COMPLETE" : xs) = COMPLETE (read (head xs) :: Int)
toAction ("GROW" : xs : _) = GROW (read xs :: Int)
toAction ("SEED" : source : target : _) = SEED (read source :: Int) (read target :: Int)

getIndex :: Action -> Int
getIndex (COMPLETE i) = i
getIndex (GROW i) = i
getIndex _ = -1

isComplete :: Action -> Bool
isComplete (COMPLETE _) = True
isComplete _ = False

isGrow :: Action -> Bool
isGrow (GROW _) = True
isGrow _ = False

type Value = Int

data Tradeoffs = Tradeoffs { _possibleAward :: Int
                           , _currentCost :: Int
} deriving Show

data TreeInfo = TreeInfo { _tree :: Tree
                         , _cell :: Cell
                         , _action :: Maybe Action
                         , _tradeoffs :: Tradeoffs
} deriving Show

data Turn = Turn { _day :: Int
                 , _nutrients :: Int
                 , _mySun :: Int
                 , _myScore :: Int
                 , _oppSun :: Int
                 , _oppScore :: Int
                 , _oppIsWaiting :: Bool
                 , _trees :: [Tree]
                 , _possibleActions :: [Action]
} deriving Show

---------------------------------------------------------------------------------------------------
----------------------------------------------- AI ------------------------------------------------
---------------------------------------------------------------------------------------------------

possibleAward :: Int -> Int -> Int
possibleAward nutrients richness = nutrients + richnessAward
  where
    richnessAward = case richness of
      1 -> 0
      2 -> 2
      3 -> 4

totalCost = undefined
currentCost = undefined

costToGrow :: [Tree] -> TreeSize -> Int
costToGrow ts s = case s of
      1 -> 3 + totalOwn
      2 -> 7 + totalOwn
      3 -> 0
  where
    totalOwn = length $ filter (\t -> _treeSize t == bigger s) ts


myTrees :: [Tree] -> [Tree]
myTrees = filter _isMine

isActionable :: TreeInfo -> Bool
isActionable (TreeInfo _ _ (Just a) _) = True
isActionable _ = False

getIndexes :: (Action -> Bool) -> [Action] -> [Int]
getIndexes f as = map getIndex $ filter f as

chooseAction :: Maybe Action -> Action
chooseAction (Just a) = a
chooseAction Nothing = WAIT

getTreeInfo :: Forest -> Turn -> Tree -> TreeInfo
getTreeInfo forest turn tree = TreeInfo tree cell action tradeoffs
  where
    treeIndex = _cellIndex tree
    cell = fromJust $ find (\c -> _index c == treeIndex) forest
    action = find (\a -> getIndex a == treeIndex) (_possibleActions turn)
    tradeoffs = Tradeoffs award cost
    award = possibleAward (_nutrients turn) (_richness cell)
    cost = costToGrow (myTrees (_trees turn)) (_treeSize tree)

goodnessScore :: TreeInfo -> Int
goodnessScore (TreeInfo _ _ _ tradeoffs) = _possibleAward tradeoffs - _currentCost tradeoffs

getAction (TreeInfo _ _ a _) = fromJust a

candidateToAction (Just (s, info)) = getAction info
candidateToAction Nothing = WAIT

doMagic :: Forest -> IO ()
doMagic forest = do
    turn <- readTurn

    debug (show turn)

    let mine = myTrees (_trees turn)

    let treesWithInfo = map (getTreeInfo forest turn) mine

    debug (show treesWithInfo)

    let scoreWithInfo = map (\i -> (goodnessScore i, i)) (filter isActionable treesWithInfo)
    let sortedByScore = sortBy (\(s1, i1) (s2, i2) -> compare s2 s1) scoreWithInfo
    debug (show sortedByScore)

    let candidate = listToMaybe sortedByScore

    debug (show candidate)

    let action = candidateToAction candidate
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
  forever $ do doMagic cells

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


treeFromString input_line = Tree cellindex size ismine isdormant
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

cellFromString input_line = Cell index richness neighbours
  where
    input = words input_line
    index = read (input !! 0) :: Int -- 0 is the center cell, the next cells spiral outwards
    richness = read (input !! 1) :: Int -- 0 if the cell is unusable, 1-3 for usable cells
    neighbours = [read (input !! i) :: Int | i <- [2..7]] -- the index of the neighbouring cell for each direction

readCells n = replicateM n $ do
    cellFromString <$> getLine