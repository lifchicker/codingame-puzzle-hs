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
            '$' -> Ref $ toInt xs
            _ -> Val $ toInt s
    where toInt y = read y :: Int

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

takeLeft x _ = x

deref m r = case (m M.! r) of
  (Value x@(Val _)) -> Just x
  (Value (Ref r2)) -> deref m r2
  _ -> Nothing

evaluateOp :: (Int -> Int -> Int) -> MapToOp -> Int -> Var -> Var -> MapToOp
evaluateOp f m k (Val v1) (Val v2) = (M.insertWith takeLeft k (value (f v1 v2)) m)

evaluateOp f m k (Ref r1) v2@(Val _) = case (deref m r1) of
  Just x -> evaluateOp f m k x v2
  _ -> evaluate (evaluate m r1) k

evaluateOp f m k v1@(Val _) (Ref r2) = case (deref m r2) of
  Just x -> evaluateOp f m k v1 x
  _ -> evaluate (evaluate m r2) k

evaluateOp f m k (Ref r1) (Ref r2) = 
  let dr1 = deref m r1
      dr2 = deref m r2
  in case (dr1, dr2) of
    (Just x, Just y) -> evaluateOp f m k x y
    _ -> evaluate (evaluate (evaluate m r1) r2) k

evaluate :: MapToOp -> Int -> MapToOp
evaluate m k = case (m M.! k) of
  (Value (Val v)) -> m
  (Value x@(Ref r)) -> evaluateOp takeLeft m k x (Val 0)
  (Add x y) -> evaluateOp (+) m k x y
  (Sub x y) -> evaluateOp (-) m k x y
  (Mult x y) -> evaluateOp (*) m k x y
  
evalSpreadsheetCells :: [Op] -> [Int]
evalSpreadsheetCells xs = map (\(Value (Val x)) -> x) (M.elems $ foldl f l (M.keys l))
  where l = lookupTable xs
        f m i = evaluate m i

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
