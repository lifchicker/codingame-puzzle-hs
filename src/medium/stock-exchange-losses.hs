-- https://www.codingame.com/training/medium/stock-exchange-losses

import System.IO
import Control.Monad

f (b,xs) a = (m, m:xs) where m = max a b
maxs xs = foldl f ((head xs), []) xs
best xs = minimum $ zipWith (-) xs (reverse $ snd $ maxs xs)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    input_line <- getLine
    let n = read input_line :: Int
    input_line <- getLine
    let l = map (\x -> read x :: Int) $ words input_line
    putStrLn (show $ best l)
    return ()