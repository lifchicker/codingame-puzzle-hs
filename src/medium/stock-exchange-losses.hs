-- https://www.codingame.com/training/medium/stock-exchange-losses

import System.IO
import Control.Monad

losses (x:[]) = [0]
losses (x:xs) = (maximum (map (x-) xs)) : (losses xs)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    input_line <- getLine
    let n = read input_line :: Int
    input_line <- getLine
    let stocks = map (\x -> read x :: Int) $ words input_line
    let best = -(maximum $ losses stocks) 
    putStrLn (show best)
    return ()