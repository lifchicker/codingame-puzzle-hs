-- https://www.codingame.com/contests/spring-challenge-2022

-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
import System.IO
    ( stdout,
      stderr,
      hSetBuffering,
      BufferMode(NoBuffering),
      hPutStrLn )
import Control.Monad ( replicateM, replicateM_ )
import Debug.Trace ( traceShowM )

-- Debug helper function to print something
debug :: (Show a) => [a] -> IO ()
debug = hPutStrLn stderr . show

type Coords = (Int, Int)

class HasCoords a where
  coords :: a -> Coords

newtype Base = Base
    { baseCoords :: Coords
    }
  deriving Show

instance HasCoords Base where
  coords = baseCoords

data Entity
    = Monster EntityData MonsterData
    | MyHero EntityData
    | EnemyHero EntityData
  deriving Show

data EntityData =
  EntityData
    { entityId :: Int
    , entityCoord :: Coords
    , entityShieldLife :: Int
    , entityIsControlled :: Bool
    }
  deriving Show

data MonsterData =
  MonsterData
    { monsterHealth :: Int
    , monsterVx :: Int
    , monsterVy :: Int
    , monsterNearBase :: Bool
    , monsterThreatFor :: ThreatFor
    }
  deriving Show

data ThreatFor = Neutral | MyBase | EnemyBase
  deriving (Show, Eq, Enum)

data Command = WAIT | MOVE Int Int
  deriving Show

move = uncurry MOVE . coords

instance HasCoords Entity where
  coords (Monster   e _) = entityCoord e
  coords (MyHero    e)   = entityCoord e
  coords (EnemyHero e)   = entityCoord e

distance e1 e2 = distance2d (coords e1) (coords e2)

distance2d (x1, y1) (x2, y2) =
  let dx = x1 - x2
      dy = y1 - y2
  in sqrt $ fromIntegral ((dx*dx) + (dy*dy))

isMyHero (MyHero _) = True
isMyHero _ = False

isAThreat (Monster _ md) = monsterThreatFor md == MyBase
isAThreat _ = False

-------------------------------------------------------------------------------
-- game loop and main logic
-------------------------------------------------------------------------------
filterMyHeroes = filter isMyHero
filterMonsters = filter isAThreat

threatLevel b h e = 17630 - distance b e - distance h e

findHighestThreat b e [x] = x
findHighestThreat b e (x:xs) = foldr (\o1 o2 -> if threatLevel b e o1 > threatLevel b e o2 then o1 else o2) x xs

nextHeroMove :: Base -> Entity -> [Entity] -> Command
nextHeroMove b h [] = WAIT
nextHeroMove b h es = move $ findHighestThreat b h es

-- simplest logic - for each hero find the closes monster who's aiming own base and move to it
-- improvement: separate monster for each hero
-- improvement: do not go far from own base

gameLoop :: Base -> Int -> IO ()
gameLoop base heroesPerPlayer = do
    [health, mana] <- readInts
    [enemyHealth, enemyMana] <- readInts
    entityCount <- readInt
    entities <- replicateM entityCount (makeEntity <$> readInts)
    -- mapM_ traceShowM entities
    let myHeros = filterMyHeroes entities
    let monsters = filterMonsters entities

    mapM_ (\h -> putStrLn $ show $ nextHeroMove base h monsters) myHeros
    gameLoop base heroesPerPlayer

-------------------------------------------------------------------------------
-- read state helper functions and main
-------------------------------------------------------------------------------
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    base <- makeBase <$> readInts
    heroesPerPlayer <- readInt
    gameLoop base heroesPerPlayer

readInt :: IO Int
readInt = readLn

readInts :: IO [Int]
readInts = map read . words <$> getLine

makeEntity :: [Int] -> Entity
makeEntity [id_, type_, x, y, shieldLife, isControlled, health, vx, vy, nearBase, threatFor] =
    let
        properties = EntityData id_ (x, y) shieldLife (isControlled == 1)
        monster = MonsterData health vx vy (nearBase == 1) (toEnum threatFor)
    in
    case type_ of
        0 -> Monster properties monster
        1 -> MyHero properties
        2 -> EnemyHero properties

makeBase :: [Int] -> Base
makeBase [baseX_, baseY_] = Base (baseX_, baseY_)
