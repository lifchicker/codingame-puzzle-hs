-- https://www.codingame.com/training/medium/mars-lander-episode-2
import Control.Monad
import System.IO

-- Acceleration is in m/s2
type Acceleration = Double

-- Speed is in m/s
type Speed = Int

-- in s
type Time = Int

-- one of 0-4
type Thrust = Int

toAcceleration :: Thrust -> Acceleration
toAcceleration = fromIntegral

toSpeed :: Time -> Acceleration -> Speed
toSpeed t acc = round (acc :: Double) * (t :: Int)

toSpeedNextMoment = toSpeed 1

safeVSpeed :: Speed
safeVSpeed = -40

gravity :: Acceleration
gravity = -3.711

type Coord = (Int, Int)

type Velocity = (Speed, Speed)

data Lander = Lander { landerCoord::Coord
                     , landerVelocity::Velocity
                     , landerFuel::Int
                     , landerRotate::Int
                     , landerPower:: Thrust
} deriving Show

data World = World { worldLander :: Lander
                   , worldGround :: [Coord]
} deriving Show

speed :: Speed -> Speed -> Thrust -> Time -> Speed
speed vs g p t = vs + toSpeed t (gravity + toAcceleration p)

-- vertical speed is probably always negative
-- idea is to maintain minimul required thrust to avoid hitting the ground very hard
getThrust :: Speed -> Thrust -> Thrust
getThrust vspeed power = case vspeed of
  vs | speed vs gravity power 4 > safeVSpeed -> 0
  vs | speed vs gravity power 3 > safeVSpeed -> 1
  vs | speed vs gravity power 2 > safeVSpeed -> 2
  vs | speed vs gravity power 1 > safeVSpeed -> 3
  vs | speed vs gravity power 1 <= safeVSpeed -> 4
  _ -> 0
  where speed vs g p t = vs + toSpeed t (gravity + toAcceleration p)

getRotation :: Coord -> Coord -> Int
getRotation c1 c2 = case distance c1 c2 of
  d | d < 1000 -> 10
  _ -> 90

getDirection :: Coord -> Coord -> Int
getDirection (x1, _) (x2, _) = if x1 <= x2 then -1 else 1

distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = abs $ x1 - x2

doMagic :: World -> IO World -> IO World
doMagic acc n = do
  new <- n
  let accLander = worldLander acc
  let newLander = worldLander new

  let thrust = getThrust (fst $ landerVelocity newLander) (landerPower newLander)
  let closest = pickClosestFlat (landerCoord newLander) $ flatGround (worldGround new)
  let rotation = getDirection (landerCoord newLander) closest * getRotation (landerCoord newLander) closest

  -- putStrLn $ format nextMoves
  putStrLn (show rotation ++ " " ++ show thrust)
  return (World newLander (worldGround acc))



isFlat :: (Coord, Coord) -> Bool
isFlat ((x1, y1), (x2, y2)) = y1 == y2

flatGround :: [Coord] -> (Coord, Coord)
flatGround ground = head flat
  where
         flat :: [(Coord, Coord)]
         flat = findFlat ground
         findFlat :: [Coord] -> [(Coord, Coord)]
         findFlat xs = filter isFlat (zip xs (tail xs))

pickClosestFlat :: Coord -> (Coord, Coord) -> Coord
pickClosestFlat l (c1, c2) = if d1 <= d2 then c1 else c2
  where
        d1 = distance l c1
        d2 = distance l c2


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  -- Auto-generated code below aims at helping you parse
  -- the standard input according to the problem statement.

  input_line <- getLine
  let surfacen = read input_line :: Int -- the number of points used to draw the surface of Mars.
  ground <- replicateM surfacen $ do
    input_line <- getLine
    let input = words input_line
    let landx = read (head input) :: Int -- X coordinate of a surface point. (0 to 6999)
    let landy = read (input !! 1) :: Int -- Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
    return ((landx, landy)::Coord)

  hPrint stderr ground

  let world = flip World ground

  -- game loop
  initialLander <- readLander
  let initialWorld = World initialLander ground
  foldM_ doMagic initialWorld (pure initialWorld : repeat (fmap world readLander))
    -- hPutStrLn stderr "Debug messages..."


readLander :: IO Lander
readLander = landerFromString <$> getLine

landerFromString input_line = Lander (x, y) (hspeed, vspeed) fuel rotate power
  where
         input = words input_line
         x = read (head input) :: Int
         y = read (input !! 1) :: Int
         hspeed = read (input !! 2) :: Speed -- the horizontal speed (in m/s), can be negative.
         vspeed = read (input !! 3) :: Speed -- the vertical speed (in m/s), can be negative.
         fuel = read (input !! 4) :: Int -- the quantity of remaining fuel in liters.
         rotate = read (input !! 5) :: Int -- the rotation angle in degrees (-90 to 90).
         power = read (input !! 6) :: Thrust -- the thrust power (0 to 4). 1 power is equivalent of 1m/s2
