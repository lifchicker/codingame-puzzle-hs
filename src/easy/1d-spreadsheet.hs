import System.IO
import Control.Monad

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
  -- Auto-generated code below aims at helping you parse
  -- the standard input according to the problem statement.
    
  input_line <- getLine
  let n = read input_line :: Int
    
  replicateM n $ do
    input_line <- getLine
    let input = words input_line
    let operation = input!!0
    let arg1 = input!!1
    let arg2 = input!!2
    return ()
    
  replicateM n $ do
        
    -- hPutStrLn stderr "Debug messages..."
        
    -- Write answer to stdout
    putStrLn "1"
    return ()
  return ()
