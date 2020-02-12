-- https://www.codingame.com/training/medium/there-is-no-spoon-episode-1

import System.IO
import Control.Monad
import qualified Data.List as L
import qualified Data.Tuple as T

findR i j xs = case (L.elemIndex '0' (drop (i + 1) xs)) of
  Just x -> (i + 1 + x, j)
  _ -> (-1, -1)

findB i j xs = case (L.elemIndex '0' (drop (j + 1) xs)) of
  Just x -> (i, j + 1 + x)
  _ -> (-1, -1)

column i c = [l !! i | l <- c]

toArray (x, y) = [x, y]

gen xs = [[(x, y), findR x y r, findB x y c] | y <- [0..length xs - 1],
                                               let r = xs !! y,
                                               x <- [0..length r - 1],
                                               let c = column x xs,
                                               (xs !! y) !! x == '0']

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
  getLine -- ignore width
  input_line <- getLine
  let height = read input_line :: Int -- the number of cells on the Y axis
    
  cells <- replicateM height $ do
    line <- getLine
    return (line)

  mapM_ putStrLn $ map unwords $ map (\l -> map show . concat . map toArray $ l) $ gen cells  

  return ()
