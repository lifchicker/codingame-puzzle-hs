-- https://www.codingame.com/contests/spring-challenge-2020
import System.IO
import Control.Monad
import Data.List
import Data.Maybe

type Coord = (Int, Int)

---------------------------------------
-- PacType
---------------------------------------
data PacType = ROCK | PAPER | SCISSORS
  deriving Show

toPacType s = case s of 
              "ROCK" -> ROCK
              "PAPER" -> PAPER
              "SCISSORS" -> SCISSORS

ROCK `beats` SCISSORS = True
SCISSORS `beats` PAPER = True
PAPER `beats` ROCK = True
beats _ _ = False

getOffenderType t = case t of 
                    SCISSORS -> ROCK
                    PAPER -> SCISSORS
                    ROCK -> PAPER


---------------------------------------
-- AbilityType
---------------------------------------
data AbilityType = SWITCH | SPEED
  deriving Show

toAbilityType s = case s of
                  "SWITCH" -> SWITCH
                  "SPEED" -> SPEED


---------------------------------------
-- Pac
---------------------------------------
type Id = Int
data Pac = Pac { pacid::Id
               , mine::Bool 
               , pacCoord::Coord
               , pacType::PacType
               , speedTurnsLeft::Int
               , abilityCooldown::Int
} deriving Show


---------------------------------------
-- Pellet
---------------------------------------
data Pellet = Pellet { pelletCoord::Coord
                     , value::Int
} deriving Show


---------------------------------------
-- World
---------------------------------------
data World = World { _pacs::[Pac]
                   , _pellets::[Pellet]
} deriving Show


---------------------------------------
-- HasCoord
---------------------------------------
class HasCoord c where
  coord :: c -> Coord

instance HasCoord Pac where
  coord (Pac _ _ c _ _ _) = c

instance HasCoord Pellet where
  coord (Pellet c _) = c

distance :: (Floating a, HasCoord c1, HasCoord c2) => c2 -> c1 -> a
distance o1 o2 = sqrt $ fromIntegral $ ((dx*dx) + (dy*dy))
  where (x1, y1) = coord o1
        (x2, y2) = coord o2
        dx = x1 - x2
        dy = y1 - y2


---------------------------------------
-- Action
---------------------------------------
data Action = Move Id Int Int
            | Switch Id PacType
            | Speed Id

instance Show Action where
  show (Move i x y) = "MOVE " ++ (show i) ++ " " ++ (show x) ++ " " ++ (show y)
  show (Switch i t) = "SWITCH " ++ (show i) ++ " " ++ (show t)
  show (Speed i) = "SPEED " ++ (show i)

type Actions = [Action]

move p t = Move (pacid p) (fst c) (snd c) where c = coord t
switch p t = Switch (pacid p) t
speed p = Speed (pacid p)


---------------------------------------
-- AI
---------------------------------------

-- list of current issues:
-- 1. few pacs aiming to same pellet -> this cause them to stuck in same moves all over again
filterMine pacs = filter mine pacs
filterEnemies pacs = filter (not.mine) pacs

findClosest p xs = foldr (\o1 o2 -> if (distance p o1) < (distance p o2) then o1 else o2) (head xs) (tail xs)
-- if distance for closest enemy is <2 and can beat -> continue
-- if distance for closest enemy is <2 and can't beat and can use ability -> change type and continue
-- else -> run away


getNextMove pac pellets = findClosest pac pellets

showNextMove pacs pellets = intercalate " | " $ (map show nextMoves)
  where myPacs = filterMine pacs
        nextMoves = map (\p -> move p (getNextMove p pellets)) myPacs


---------------------------------------
-- MAIN --
---------------------------------------
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
  return (Pac pacid mine (x,y) (toPacType typeid) speedturnsleft abilitycooldown)

readPellets = do
  input_line <- getLine
  -- hPutStrLn stderr input_line
  let input = words input_line
  let x = read (input!!0) :: Int
  let y = read (input!!1) :: Int
  let value = read (input!!2) :: Int -- amount of points this pellet is worth
  return (Pellet (x,y) value)
