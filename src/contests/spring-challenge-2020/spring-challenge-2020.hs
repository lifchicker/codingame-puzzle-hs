-- https://www.codingame.com/contests/spring-challenge-2020
import System.IO
import Control.Monad
import Data.List
import Data.Maybe

type Coord = (Int, Int)

class HasCoord c where
  coord :: c -> Coord

data Pac = Pac { pacid::Int
               , mine::Bool 
               , _pacCoord::Coord
} deriving Show

instance HasCoord Pac where
  coord (Pac _ _ c) = c

data Pellet = Pellet { _pelletCoord::Coord
                     , _value::Int
} deriving Show

instance HasCoord Pellet where
  coord (Pellet c _) = c

data World = World { _pacs::[Pac]
                   , _pellets::[Pellet]
} deriving Show


distance :: (Floating a, HasCoord c1, HasCoord c2) => c2 -> c1 -> a
distance o1 o2 = sqrt $ fromIntegral $ ((dx*dx) + (dy*dy))
  where (x1, y1) = coord o1
        (x2, y2) = coord o2
        dx = x1 - x2
        dy = y1 - y2


findMine pacs = filter mine pacs

findClosestPellet pac pellets = foldr (\p1 p2 -> if (distance pac p1) < (distance pac p2) then p1 else p2) (head pellets) (tail pellets)

getNextMove pac pellets = findClosestPellet pac pellets

showMove i (x,y) = "MOVE " ++ (show i) ++ " " ++ (show x) ++ " " ++ (show y)

showNextMove pacs pellets = intercalate " | " nextMoves
  where myPacs = findMine pacs
        nextMoves = map (\p -> showMove (pacid p) (_pelletCoord $ getNextMove p pellets)) myPacs


-- MAIN --
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  -- Grab the pellets as fast as you can!

  input_line <- getLine
  -- hPutStrLn stderr input_line
  let input = words input_line
  let width = read (input!!0) :: Int -- size of the grid
  let height = read (input!!1) :: Int -- top left corner is (x=0, y=0)

  -- ignore for now as we will implement simplies algo for start
  replicateM height $ do
    row <- getLine
    -- one line of the grid: space " " is floor, pound "#" is wall
    -- hPutStrLn stderr row
    return ()

  -- game loop
  forever $ do
    input_line <- getLine
    -- hPutStrLn stderr input_line
    let input = words input_line
    let myscore = read (input!!0) :: Int
    let opponentscore = read (input!!1) :: Int
    input_line <- getLine
    -- hPutStrLn stderr input_line
    let visiblepaccount = read input_line :: Int -- all your pacs and enemy pacs in sight

    pacs <- replicateM visiblepaccount $ readPacs
    -- putStrLn (show pacs)

    input_line <- getLine
    -- hPutStrLn stderr input_line
    let visiblepelletcount = read input_line :: Int -- all pellets in sight

    pellets <- replicateM visiblepelletcount $ readPellets

    -- MOVE <pacId> <x> <y>
    putStrLn $ showNextMove pacs pellets

readPacs = do
  input_line <- getLine
  -- hPutStrLn stderr input_line
  let input = words input_line
  let pacid = read (input!!0) :: Int -- pac number (unique within a team)
  let mine = (read (input!!1) :: Int) == 1 -- true if this pac is yours
  let x = read (input!!2) :: Int -- position in the grid
  let y = read (input!!3) :: Int -- position in the grid
  let typeid = input!!4 -- unused in wood leagues
  let speedturnsleft = read (input!!5) :: Int -- unused in wood leagues
  let abilitycooldown = read (input!!6) :: Int -- unused in wood leagues
  return (Pac pacid mine (x,y) )

readPellets = do
  input_line <- getLine
  -- hPutStrLn stderr input_line
  let input = words input_line
  let x = read (input!!0) :: Int
  let y = read (input!!1) :: Int
  let value = read (input!!2) :: Int -- amount of points this pellet is worth
  return (Pellet (x,y) value)
