import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import           Data.Set (Set, fromList, member, delete)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  input_line <- getLine
  let input = words input_line
  let width = read (input!!0) :: Int -- size of the grid
  let height = read (input!!1) :: Int -- top left corner is (x=0, y=0)

  grid <- readGrid height
  let notVisited = fromList $ toCoord height $ map toFloorXs grid
  putStrLn $ show notVisited
  putStrLn $ show (member (14,1) notVisited)
  return ()

toFloorXs xs = map fst $ filter (\(x, c) -> c == ' ')  $ zip [0..] xs
toCoord h xs = concat $ zipWith (\y xs -> zip xs (repeat y)) [0..h] xs

readGrid height = replicateM height $ do
  row <- getLine
  -- one line of the grid: space " " is floor, pound "#" is wall
  hPutStrLn stderr row
  return (row)
