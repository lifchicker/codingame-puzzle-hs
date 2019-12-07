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

  --let xs = [Sub (Ref 47) (Ref 9),Sub (Val 44) (Ref 59),Add (Ref 97) (Ref 67),Add (Ref 1) (Ref 1),Sub (Ref 57) (Ref 67),Add (Ref 47) (Ref 97),Add (Ref 59) (Ref 59),Sub (Ref 50) (Ref 83),Sub (Ref 3) (Ref 93),Sub (Ref 4) (Ref 74),Sub (Ref 38) (Ref 0),Add (Ref 29) (Ref 96),Sub (Ref 46) (Ref 97),Sub (Ref 5) (Ref 98),Sub (Ref 87) (Ref 66),Sub (Ref 86) (Ref 25),Sub (Ref 1) (Ref 98),Sub (Ref 84) (Ref 56),Add (Ref 38) (Ref 78),Add (Ref 46) (Ref 34),Add (Ref 5) (Ref 76),Sub (Ref 3) (Ref 93),Add (Ref 19) (Ref 31),Add (Ref 97) (Ref 77),Value (Ref 54),Sub (Ref 6) (Ref 6),Add (Ref 98) (Ref 2),Add (Ref 59) (Ref 67),Sub (Ref 36) (Ref 86),Sub (Ref 98) (Ref 26),Sub (Ref 16) (Ref 7),Value (Ref 67),Add (Ref 11) (Ref 84),Value (Ref 63),Add (Ref 3) (Ref 6),Value (Ref 44),Sub (Ref 68) (Ref 5),Add (Ref 7) (Ref 58),Add (Ref 50) (Ref 82),Add (Ref 88) (Val (-936)),Add (Ref 43) (Ref 47),Add (Ref 58) (Val 842),Sub (Ref 80) (Ref 46),Sub (Ref 33) (Ref 96),Sub (Ref 43) (Ref 46),Add (Ref 2) (Ref 8),Add (Ref 59) (Ref 9),Value (Ref 2),Sub (Ref 65) (Ref 30),Add (Val 135) (Ref 65),Add (Ref 71) (Ref 93),Add (Ref 96) (Ref 67),Add (Ref 6) (Ref 38),Sub (Ref 5) (Ref 8),Sub (Ref 67) (Ref 1),Add (Ref 4) (Ref 71),Value (Ref 67),Sub (Ref 93) (Ref 54),Sub (Ref 51) (Ref 3),Add (Val 993) (Val (-871)),Add (Ref 6) (Ref 6),Sub (Ref 71) (Ref 65),Add (Ref 25) (Ref 60),Value (Ref 59),Add (Ref 6) (Ref 51),Sub (Ref 63) (Ref 97),Value (Ref 67),Sub (Val 3) (Ref 59),Add (Ref 88) (Ref 3),Sub (Ref 83) (Ref 53),Sub (Ref 50) (Ref 49),Add (Ref 60) (Val 865),Value (Ref 53),Sub (Ref 29) (Ref 44),Sub (Ref 96) (Ref 25),Add (Ref 21) (Ref 77),Sub (Ref 14) (Ref 30),Sub (Ref 27) (Ref 50),Add (Ref 51) (Ref 5),Sub (Ref 40) (Ref 72),Value (Ref 90),Add (Ref 87) (Ref 42),Add (Ref 9) (Ref 47),Sub (Ref 97) (Ref 1),Add (Ref 21) (Ref 44),Add (Ref 78) (Ref 94),Add (Ref 21) (Ref 71),Add (Val (-730)) (Ref 67),Sub (Ref 21) (Ref 89),Sub (Ref 83) (Ref 25),Add (Ref 47) (Ref 84),Add (Ref 6) (Ref 65),Add (Ref 32) (Ref 22),Add (Ref 27) (Ref 59),Add (Ref 63) (Ref 11),Add (Ref 65) (Ref 60),Add (Ref 59) (Ref 6),Sub (Ref 1) (Ref 27),Add (Ref 27) (Ref 83),Sub (Ref 19) (Ref 61)]
  mapM_ (putStrLn.show) (evalSpreadsheetCells cells)

  return ()
