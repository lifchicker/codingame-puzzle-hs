-- https://www.codingame.com/contests/spring-challenge-2021
import Control.Monad
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  -- Auto-generated code below aims at helping you parse
  -- the standard input according to the problem statement.

  input_line <- getLine
  let numberofcells = read input_line :: Int -- 37
  replicateM numberofcells $ do
    input_line <- getLine
    let input = words input_line
    let index = read (input !! 0) :: Int -- 0 is the center cell, the next cells spiral outwards
    let richness = read (input !! 1) :: Int -- 0 if the cell is unusable, 1-3 for usable cells
    let neigh0 = read (input !! 2) :: Int -- the index of the neighbouring cell for each direction
    let neigh1 = read (input !! 3) :: Int
    let neigh2 = read (input !! 4) :: Int
    let neigh3 = read (input !! 5) :: Int
    let neigh4 = read (input !! 6) :: Int
    let neigh5 = read (input !! 7) :: Int
    return ()

  -- game loop
  forever $ do
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
    replicateM numberoftrees $ do
      input_line <- getLine
      let input = words input_line
      let cellindex = read (input !! 0) :: Int -- location of this tree
      let size = read (input !! 1) :: Int -- size of this tree: 0-3
      let ismine = read (input !! 2) == 1 :: Bool -- 1 if this is your tree
      let isdormant = read (input !! 3) == 1 :: Bool -- 1 if this tree is dormant
      return ()
    input_line <- getLine
    let numberofpossibleactions = read input_line :: Int -- all legal actions
    replicateM numberofpossibleactions $ do
      possibleaction <- getLine
      -- try printing something from here to start with
      return ()

    -- hPutStrLn stderr "Debug messages..."

    -- GROW cellIdx | SEED sourceIdx targetIdx | COMPLETE cellIdx | WAIT <message>

    putStrLn "WAIT"
