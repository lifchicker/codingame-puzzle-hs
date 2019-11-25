-- https://www.codingame.com/ide/puzzle/1d-spreadsheet

import System.IO
import Control.Monad
import qualified Data.Map as M

data Var = Val Int
         | Ref Int
         | None
  deriving (Eq, Show)

data Op = Value Var
        | Add Var Var
        | Sub Var Var
        | Mult Var Var
  deriving (Eq, Show)

value :: Int -> Op
value x = Value $ Val x

strToVar :: String -> Var
strToVar s@(x:xs) = 
  case x of '_' -> None
            '$' -> Ref $ sToI xs
            _ -> Val $ sToI s
    where sToI y = read y :: Int

strToOp :: String -> Var -> Var -> Op
strToOp "VALUE" x None = Value x
strToOp "ADD" x y = Add x y
strToOp "SUB" x y = Sub x y
strToOp "MULT" x y = Mult x y

indexed :: [b] -> [(Int, b)]
indexed = zipWith (,) [0..]

lookupTable :: [a] -> M.Map Int a
lookupTable e = M.fromList $ indexed e

type MapToOp = M.Map Int Op

eval :: MapToOp -> Var -> (MapToOp, Int)
eval m (Val v) = (m, v)
eval m (Ref r) = ((M.insertWith (\x _ -> x) r (value o) m1), o)
  where (m1, o) = evaluate m (m M.! r)

evaluate :: MapToOp -> Op -> (MapToOp, Int)
evaluate m (Value v) = (m1, l)
  where (m1, l) = eval m v

evaluate m (Add x y) = (m2, l + r)
  where (m1, l) = eval m x
        (m2, r) = eval m2 y

evaluate m (Sub x y) = (m2, l - r)
  where (m1, l) = eval m x
        (m2, r) = eval m2 y

evaluate m (Mult x y) = (m2, l * r)
  where (m1, l) = eval m x
        (m2, r) = eval m2 y


evalSpreadsheetCells :: [Op] -> [Int]
evalSpreadsheetCells xs = foldl' (evaluate l) xs
  where l = lookupTable xs

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
  input_line <- getLine
  let n = read input_line :: Int
    
  cells <- replicateM n $ do
    input_line <- getLine
    let input = words input_line
    let cell = strToOp (input!!0) (strToVar $ input!!1) (strToVar $ input!!2)
    return (cell)
  
  hPutStrLn stderr (show cells)

  mapM_ (putStrLn.show) (evalSpreadsheetCells cells)

  return ()
