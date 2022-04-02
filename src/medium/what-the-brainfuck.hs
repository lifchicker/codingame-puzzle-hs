-- https://www.codingame.com/training/medium/what-the-brainfuck

-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
import Control.Monad (replicateM)
import Data.Maybe (catMaybes)
import Data.Char (chr, ord)
import System.IO
  ( BufferMode (NoBuffering),
    hPutStrLn,
    hSetBuffering,
    stderr,
    stdout,
  )

-- Debug helper function to print something
debug :: (Show a) => [a] -> IO ()
debug = hPutStrLn stderr . show

data Tape a = Tape [a] a [a]

moveRight :: Show a => Tape a -> Maybe (Tape a)
moveRight (Tape _  _ []) = Nothing
moveRight tape = Just $ moveUncheckedRight tape

moveUncheckedRight :: Show a => Tape a -> Tape a
moveUncheckedRight (Tape ls v (r:rs)) = Tape (v:ls) r rs

moveLeft :: Tape a -> Maybe (Tape a)
moveLeft (Tape [] _  _) = Nothing
moveLeft tape = Just $ moveUncheckedLeft tape

moveUncheckedLeft :: Tape a -> Tape a
moveUncheckedLeft (Tape (l:ls) v rs) = Tape ls l (v:rs)

updateValue :: a -> Tape a -> Tape a
updateValue v tape@(Tape ls _ rs) = Tape ls v rs

type Inputs = Tape Int
type MemoryTape = Tape Int
type Commands = Tape Command

instance (Show a) => Show (Tape a) where
  show (Tape ls v rs) = show ls ++ " " ++ show v ++ " " ++ show rs

tapify :: [a] -> Tape a
tapify (x:xs) = Tape [] x xs

data Command = Next | Previous | Increment | Decrement | Print | Read | JumpForward | JumpBackward
  deriving (Show, Eq)

toCommand c = case c of
  '>' -> Just Next
  '<' -> Just Previous
  '+' -> Just Increment
  '-' -> Just Decrement
  '.' -> Just Print
  ',' -> Just Read
  '[' -> Just JumpForward
  ']' -> Just JumpBackward
  _ -> Nothing

checkSyntax :: [Command] -> Either String [Command]
checkSyntax cs = if checkJumps 0 cs then Right cs else Left "SYNTAX ERROR"
  where
    checkJumps c [] = c == 0
    checkJumps c (JumpForward:xs) = c >= 0 && checkJumps (c + 1) xs
    checkJumps c (JumpBackward:xs) = c >= 0 && checkJumps (c - 1) xs
    checkJumps c (_:xs) = checkJumps c xs

increment :: MemoryTape -> Maybe MemoryTape
increment (Tape ls v rs) = if v == 255 then Nothing else Just $ Tape ls (v + 1) rs

decrement :: MemoryTape -> Maybe MemoryTape
decrement (Tape ls v rs) = if v == 0 then Nothing else Just $ Tape ls (v - 1) rs

moveMemoryTapeOrError :: Inputs -> Maybe MemoryTape -> Commands -> Either String String
moveMemoryTapeOrError = executeNextCommandOrError "POINTER OUT OF BOUNDS"

modifyMemoryTapeOrError :: Inputs -> Maybe MemoryTape -> Commands -> Either String String
modifyMemoryTapeOrError = executeNextCommandOrError "INCORRECT VALUE"

executeNextCommandOrError :: String -> Inputs -> Maybe MemoryTape -> Commands -> Either String String
executeNextCommandOrError error inputs x commands = case x of
  Just newMemoryTape -> executeNextCommand inputs newMemoryTape commands
  Nothing -> Left error

jumpForward :: Commands -> Commands
jumpForward commands = findPairedJumpBackward 0 commands
  where
    findPairedJumpBackward 1 commands@(Tape _ JumpBackward _) = commands
    findPairedJumpBackward c commands@(Tape _ JumpForward _)  = findPairedJumpBackward (c + 1) (moveUncheckedRight commands)
    findPairedJumpBackward c commands@(Tape _ JumpBackward _) = findPairedJumpBackward (c - 1) (moveUncheckedRight commands)
    findPairedJumpBackward c commands                         = findPairedJumpBackward c       (moveUncheckedRight commands)

jumpBackward :: Commands -> Commands
jumpBackward commands = findPairedJumpForward 0 commands
  where
    findPairedJumpForward 1 commands@(Tape _ JumpForward _)  = commands
    findPairedJumpForward c commands@(Tape _ JumpBackward _) = findPairedJumpForward (c + 1) (moveUncheckedLeft commands)
    findPairedJumpForward c commands@(Tape _ JumpForward _)  = findPairedJumpForward (c - 1) (moveUncheckedLeft commands)
    findPairedJumpForward c commands                         = findPairedJumpForward c       (moveUncheckedLeft commands)

executeNextCommand :: Inputs -> MemoryTape -> Commands -> Either String String
executeNextCommand inputs memoryTape commands = case moveRight commands of
    Just newCommands -> execute inputs memoryTape newCommands
    Nothing -> Right ""

execute :: Inputs -> MemoryTape -> Commands -> Either String String
execute inputs memoryTape              commands@(Tape _ Previous _)     = moveMemoryTapeOrError inputs (moveLeft memoryTape) commands
execute inputs memoryTape              commands@(Tape _ Next _)         = moveMemoryTapeOrError inputs (moveRight memoryTape) commands
execute inputs memoryTape              commands@(Tape _ Increment _)    = modifyMemoryTapeOrError inputs (increment memoryTape) commands
execute inputs memoryTape              commands@(Tape _ Decrement _)    = modifyMemoryTapeOrError inputs (decrement memoryTape) commands
execute inputs memoryTape@(Tape _ v _) commands@(Tape _ Print _)        = (++) <$> Right [chr v] <*> executeNextCommand inputs memoryTape commands
execute inputs memoryTape@(Tape _ 0 _) commands@(Tape _ JumpForward _)  = executeNextCommand inputs memoryTape (jumpForward commands)
execute inputs memoryTape              commands@(Tape _ JumpForward _)  = executeNextCommand inputs memoryTape commands
execute inputs memoryTape@(Tape _ 0 _) commands@(Tape _ JumpBackward _) = executeNextCommand inputs memoryTape commands
execute inputs memoryTape              commands@(Tape _ JumpBackward _) = executeNextCommand inputs memoryTape (jumpBackward commands)
execute inputs@(Tape _ v _) memoryTape commands@(Tape _ Read _)         = executeNextCommand (moveUncheckedRight inputs) (updateValue v memoryTape) commands

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE
  input_line <- getLine
  hPutStrLn stderr input_line

  let input = words input_line
  let line_count = read (head input) :: Int
  let tape_length = read (input !! 1) :: Int
  let inputs_count = read (input !! 2) :: Int

  commands <- fmap (catMaybes . concat) $ replicateM line_count $ readAndParse (map toCommand)
  debug commands

  inputs <- replicateM inputs_count $ do
    input_line <- getLine
    let c = read input_line :: Int
    return c
  debug inputs

  putStrLn $ case checkSyntax commands of
    Left e -> e
    Right cs -> case execute (tapify inputs) (tapify $ replicate tape_length 0) (tapify cs) of
      Right v -> v
      Left v -> v

  return ()

readAndParse parser = do
  input_line <- getLine
  hPutStrLn stderr input_line
  return (parser input_line)
