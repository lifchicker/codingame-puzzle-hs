-- https://www.codingame.com/contests/spring-challenge-2020
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as S
import           Data.Set (Set, fromList, member, empty)


type Coord = (Int, Int)
type Id = Int

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

getOffenderTypeAgainst p = getOffenderType (pacType p)
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
data Pac = Pac { pacid::Id
               , mine::Bool 
               , pacCoord::Coord
               , pacType::PacType
               , speedTurnsLeft::Int
               , abilityCooldown::Int
} deriving Show

canUseAbility p = (abilityCooldown p) == 0

canBeat p1 p2 = (pacType p1) `beats` (pacType p2)


---------------------------------------
-- Pellet
---------------------------------------
data Pellet = Pellet { pelletCoord::Coord
                     , value::Int
} deriving Show


---------------------------------------
-- World
---------------------------------------
data World = World { notVisited::Set Coord
                   , _pacs::[Pac]
                   , _pellets::[Pellet]
} deriving Show

withNotVisited (World n p pe) nv = World nv p pe

data Grid = Grid { width::Int
                 , height::Int
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

distance o1 o2 = distance2d (coord o1) (coord o2)

distance2d (x1, y1) (x2, y2) = 
  let dx = x1 - x2
      dy = y1 - y2
  in sqrt $ fromIntegral $ ((dx*dx) + (dy*dy))

isClose o1 o2 = (distance o1 o2) < 2

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

move p t = Move (pacid p) (fst t) (snd t)
speed p = Speed (pacid p)

switch p t = Switch (pacid p) t
switchAgains p t = switch p $ getOffenderTypeAgainst t

-------------------------------------------------------------------------------
------------------------------------- AI --------------------------------------
-------------------------------------------------------------------------------

filterMine pacs = filter mine pacs
filterEnemies pacs = filter (not.mine) pacs

findClosestP p (x:xs) = foldr (\o1 o2 -> if (distance p o1) < (distance p o2) then o1 else o2) x xs
findClosestC p (x:xs) = foldr (\o1 o2 -> if (distance2d p o1) < (distance2d p o2) then o1 else o2) x xs

-- Simple fight strategy:
--    if distance for closest enemy is <2 and can't beat with current ability and can use ability -> change type
--    else -> run away (just move for now)
switchIfCanBeat p e = if and [isClose p e, not (canBeat p e), canUseAbility p] then Just (switchAgains p e) else Nothing

pickFightStrategy p [] = []
pickFightStrategy p es = case strategies of
                         Just s -> [s]
                         Nothing -> [] 
  where e = findClosestP p es
        strategies = msum [switchIfCanBeat p e]

pickSpeedStrategy p [] = if and [canUseAbility p] then [speed p] else []
pickSpeedStrategy p _ = []

-- list of current issues:
-- 1. problem: few pacs aiming to same pellet -> this cause them to stuck in same moves all over again
--    solution: 
-- possible improvements:
-- 1. prioritize big pellets over small ones

nextMove :: [Coord] -> Pac -> [Pellet] -> Action
-- when no visibile pellets -> pick one from this list of not visited
nextMove nv p [] = move p (findClosestC (coord p) nv)
nextMove _ p pes = move p $ coord $ head $ (closestBig p $ filterClosestBigPallets p pes) ++ [closestSmall p pes]

filterClosestBigPallets p pes = filter (\x -> ((value x == 10) && (distance p x < 5))) pes
closestSmall p pes = findClosestP p pes
closestBig p [] = []
closestBig p pes = [findClosestP p pes]

nextAction nv p ps es = head $ (pickFightStrategy p es) ++ (pickSpeedStrategy p es) ++ [nextMove nv p ps]

calculateNextMoves notVisited pacs pellets = 
  let myPacs = filterMine pacs
      enemies = filterEnemies pacs
  in map (\p -> nextAction notVisited p pellets enemies) myPacs

format xs = intercalate " | " $ (map show xs)

visited nv px = foldl' (\xs c -> S.delete c xs) nv (map pacCoord px)

updateWorld acc w = 
  let updateNotVisited = visited (notVisited acc) (_pacs w)
  in withNotVisited w updateNotVisited

doMagic :: World -> IO World -> IO World
doMagic acc n = do
  new <- n
  let updated = updateWorld acc new
  hPutStrLn stderr $ show updated
  let nextMoves = calculateNextMoves (S.toList $ notVisited updated) (_pacs updated) (_pellets updated)
  putStrLn $ format nextMoves
  return (updated)


-------------------------------------------------------------------------------
------------------------------------ MAIN -------------------------------------
-------------------------------------------------------------------------------
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  -- Grab the pellets as fast as you can!

  input_line <- getLine
  hPutStrLn stderr input_line
  let input = words input_line
  let width = read (input!!0) :: Int -- size of the grid
  let height = read (input!!1) :: Int -- top left corner is (x=0, y=0)

  grid <- readGrid height
  let notVisited = fromList $ toCoord height $ map toFloorXs grid :: Set Coord

  initial <- readNewWorldState notVisited
  -- game loop
  foldM_ doMagic initial (pure initial : (repeat $ readNewWorldState empty))
  return ()
    

toFloorXs xs = map fst $ filter (\(x, c) -> c == ' ')  $ zip [0..] xs
toCoord h xs = concat $ zipWith (\y xs -> zip xs (repeat y)) [0..h] xs

readGrid height = replicateM height $ do
  row <- getLine
  -- one line of the grid: space " " is floor, pound "#" is wall
  hPutStrLn stderr row
  return (row)


readNewWorldState xs = do
  readScore
  pacs <- readPacs
  pellets <- readPellets

  return (World xs pacs pellets)


readScore = do
  input_line <- getLine
  hPutStrLn stderr input_line
  let input = words input_line
  let myscore = read (input!!0) :: Int
  let opponentscore = read (input!!1) :: Int
  return ()


readPacs = do
  input_line <- getLine
  hPutStrLn stderr input_line
  let visiblepaccount = read input_line :: Int -- all your pacs and enemy pacs in sight

  pacs <- replicateM visiblepaccount $ readAndParse pacFromString
  return (pacs)


pacFromString input_line = Pac pacid mine (x,y) (toPacType typeid) speedturnsleft abilitycooldown
  where
        input = words input_line
        pacid = read (input!!0) :: Int -- pac number (unique within a team)
        mine = (read (input!!1) :: Int) == 1 -- true if this pac is yours
        x = read (input!!2) :: Int -- position in the grid
        y = read (input!!3) :: Int -- position in the grid
        typeid = input!!4 -- the pac's type (ROCK or PAPER or SCISSORS)
        speedturnsleft = read (input!!5) :: Int -- the number of remaining turns before the speed effect fades
        abilitycooldown = read (input!!6) :: Int -- the number of turns until you can request a new ability for this pac (SWITCH and SPEED)


readPellets = do
  input_line <- getLine
  hPutStrLn stderr input_line
  let visiblepelletcount = read input_line :: Int -- all pellets in sight

  pellets <- replicateM visiblepelletcount $ readAndParse pelletFromString
  return (pellets)


pelletFromString input_line = Pellet (x,y) value
  where
        input = words input_line
        x = read (input!!0) :: Int
        y = read (input!!1) :: Int
        value = read (input!!2) :: Int -- amount of points this pellet is worth
  

readAndParse parser = do
  input_line <- getLine
  hPutStrLn stderr input_line
  return (parser input_line)
