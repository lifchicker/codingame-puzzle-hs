-- https://www.codingame.com/training/easy/mars-lander-episode-1
import Control.Monad
import System.IO ( hSetBuffering, stdout, BufferMode(NoBuffering) )

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

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  -- Auto-generated code below aims at helping you parse
  -- the standard input according to the problem statement.

  input_line <- getLine
  let surfacen = read input_line :: Int -- the number of points used to draw the surface of Mars.
  replicateM surfacen $ do
    input_line <- getLine
    let input = words input_line
    let landx = read (input !! 0) :: Int -- X coordinate of a surface point. (0 to 6999)
    let landy = read (input !! 1) :: Int -- Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
    return ()

  -- game loop
  forever $ do
    input_line <- getLine
    let input = words input_line
    let x = read (input !! 0) :: Int
    let y = read (input !! 1) :: Int
    let hspeed = read (input !! 2) :: Speed -- the horizontal speed (in m/s), can be negative.
    let vspeed = read (input !! 3) :: Speed -- the vertical speed (in m/s), can be negative.
    let fuel = read (input !! 4) :: Int -- the quantity of remaining fuel in liters.
    let rotate = read (input !! 5) :: Int -- the rotation angle in degrees (-90 to 90).
    let power = read (input !! 6) :: Thrust -- the thrust power (0 to 4). 1 power is equivalent of 1m/s2

    -- hPutStrLn stderr "Debug messages..."

    -- 2 integers: rotate power. rotate is the desired rotation angle (should be 0 for level 1), power is the desired thrust power (0 to 4).
    let thrust = getThrust vspeed power

    putStrLn ("0 " ++ show thrust)
