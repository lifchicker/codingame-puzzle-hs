-- https://www.codingame.com/contests/spring-challenge-2020
import System.IO
import Control.Monad

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
  -- Grab the pellets as fast as you can!
    
  input_line <- getLine
  let input = words input_line
  let width = read (input!!0) :: Int -- size of the grid
  let height = read (input!!1) :: Int -- top left corner is (x=0, y=0)
    
  replicateM height $ do
    row <- getLine
    -- one line of the grid: space " " is floor, pound "#" is wall
    return ()
    
  -- game loop
  forever $ do
    input_line <- getLine
    let input = words input_line
    let myscore = read (input!!0) :: Int
    let opponentscore = read (input!!1) :: Int
    input_line <- getLine
    let visiblepaccount = read input_line :: Int -- all your pacs and enemy pacs in sight
        
    replicateM visiblepaccount $ do
      input_line <- getLine
      let input = words input_line
      let pacid = read (input!!0) :: Int -- pac number (unique within a team)
      let mine = read (input!!1) :: Int -- true if this pac is yours
      let x = read (input!!2) :: Int -- position in the grid
      let y = read (input!!3) :: Int -- position in the grid
      let typeid = input!!4 -- unused in wood leagues
      let speedturnsleft = read (input!!5) :: Int -- unused in wood leagues
      let abilitycooldown = read (input!!6) :: Int -- unused in wood leagues
      return ()
    
    input_line <- getLine
    let visiblepelletcount = read input_line :: Int -- all pellets in sight
        
    replicateM visiblepelletcount $ do
      input_line <- getLine
      let input = words input_line
      let x = read (input!!0) :: Int
      let y = read (input!!1) :: Int
      let value = read (input!!2) :: Int -- amount of points this pellet is worth
      return ()
        
    -- hPutStrLn stderr "Debug messages..."
        
    -- MOVE <pacId> <x> <y>
    putStrLn "MOVE 0 15 10"