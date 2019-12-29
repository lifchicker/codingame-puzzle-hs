import System.IO
import Control.Monad
import Control.Applicative

data BoundingBox = BoundingBox { minX::Int
                               , minY::Int
                               , maxX::Int
                               , maxY::Int
 } deriving Show

data Batman = Batman {_x::Int, _y::Int} deriving Show

data State = State BoundingBox Batman

showBatman (State _ (Batman x y)) = (show x) ++ " " ++ (show y)

next min max =  round (x0 + ((x1 - x0)/c))
  where c = 2.0
        x0 = fromIntegral min
        x1 = fromIntegral max

jumpRight (State bb b) = State bb1 b1
  where x = next (_x b) (maxX bb)
        bb1 = bb {minX = (_x b)}
        b1 = b {_x = x}

jumpLeft (State bb b) = State bb1 b1
  where x = next (minX bb) (_x b)
        bb1 = bb {maxX = (_x b)}
        b1 = b {_x = x}

jumpDown (State bb b) = State bb1 b1
  where y = next (_y b) (maxY bb)
        bb1 = bb {minY = (_y b)}
        b1 = b {_y = y}

jumpUp (State bb b) = State bb1 b1
  where y = next (minY bb) (_y b)
        bb1 = bb {maxY = (_y b)}
        b1 = b {_y = y}

pickDirection 'R' = jumpRight
pickDirection 'L' = jumpLeft
pickDirection 'U' = jumpUp
pickDirection 'D' = jumpDown
pickDirection x = \x -> x

findWindow s xs = foldr pickDirection s xs

findBomb s = do
  input_line <- getLine
  let bombdir = input_line :: String
  hPutStrLn stderr bombdir
  let state = findWindow s bombdir
  putStrLn $ showBatman state
  findBomb state

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    let field = BoundingBox (-1) (-1) (read (input!!0)) (read (input!!1))
    hPutStrLn stderr (show field)
    input_line <- getLine
    let n = read input_line :: Int -- maximum number of turns before game over.
    input_line <- getLine
    let input = words input_line
    let batman = Batman (read (input!!0)) (read (input!!1))
    hPutStrLn stderr (show batman)
    
    findBomb $ State field batman
