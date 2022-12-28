-- https://www.codingame.com/contests/spring-challenge-2022

-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}

import System.IO
    ( stdout,
      stderr,
      hSetBuffering,
      BufferMode(NoBuffering),
      hPutStrLn )
import Control.Monad ( replicateM, foldM_ )
import qualified Data.Map as M
import Data.Tree ( Tree(Node, rootLabel) )
import qualified Data.Vector as V
import Data.List (sortBy)
import Debug.Trace


-- Debug helper function to print something
debug :: (Show a) => a -> IO ()
debug = hPutStrLn stderr . show

mapWidth = 17630
mapHeight = 9000

safeDistance = 8000 -- half the distance across the map

type EntityId = Int
type Coords = (Int, Int)

class HasCoords a where
  coords :: a -> Coords

class HasEntityId a where
  getId :: a -> EntityId

-------------------------------------------------------------------------------
-- Base
-------------------------------------------------------------------------------
data Base = Base
    { baseCoords :: Coords
    , baseHealth :: Int
    , baseMana :: Int
    }
  deriving Show

instance HasCoords Base where
  coords = baseCoords

updateBase :: Base -> Int -> Int -> Base
updateBase (Base c _ _) = Base c

enemyBaseCoords :: Base -> Coords
enemyBaseCoords b = bases M.! coords b
  where
    bases = M.fromList [((0, 0), (17630, 9000)), ((17630, 9000), (0, 0))]

-------------------------------------------------------------------------------
-- Entity
-------------------------------------------------------------------------------
data World =
  World
    { worldBase :: Base
    , worldMyHeroes :: V.Vector Entity
    , worldMonsters :: V.Vector Entity
    , worldEnemyHeroes :: [Entity]
    }

data Entity
    = Monster EntityData MonsterData
    | MyHero EntityData (Maybe Command)
    | EnemyHero EntityData

data EntityData =
  EntityData
    { entityId :: EntityId
    , entityCoord :: Coords
    , entityShieldLife :: Int
    , entityIsControlled :: Bool
    }

data MonsterData =
  MonsterData
    { monsterHealth :: Int
    , monsterV :: Coords
    , monsterNearBase :: Bool
    , monsterThreatFor :: ThreatFor
    }

instance HasCoords Entity where
  coords (Monster   e _)  = entityCoord e
  coords (MyHero    e _)  = entityCoord e
  coords (EnemyHero e)    = entityCoord e

instance HasEntityId Entity where
  getId (Monster   e _) = entityId e
  getId (MyHero    e _) = entityId e
  getId (EnemyHero e)   = entityId e

data ThreatFor = Neutral | MyBase | EnemyBase
  deriving (Show, Eq, Enum)

data HeroActivity = Patrol | Attack | Redirect
  deriving (Show, Eq)

data Command = WAIT | MOVE Int Int | WIND Int Int | SHIELD Entity | CONTROL Entity Int Int

-------------------------------------------------------------------------------
-- show
-------------------------------------------------------------------------------
instance Show World where
  show (World wb wmh wm weh) = "World " ++ "(" ++ show wb ++ ")" ++ show wmh ++ show wm ++ show weh

instance Show Entity where
  show (Monster ed md)  = "Monster " ++ "(" ++ show ed ++ ") (" ++ show md ++ ")"
  show (MyHero ed c)    = "MyHero " ++ "(" ++  show ed ++ ") (" ++ show c ++ ")"
  show (EnemyHero ed)   = "EnemyHero " ++ "(" ++ show ed ++ ")"

instance Show EntityData where
  show (EntityData eid ec esl eic) = unwords ["EntityData", show eid, show ec, show esl, show eic]

instance Show MonsterData where
  show (MonsterData mh mv mnb mtf) = unwords ["MonsterData", show mh, show mv, show mnb, show mtf]

instance Show Command where
  show WAIT             = "WAIT"
  show (MOVE x y)       = "MOVE " ++ show x ++ " " ++ show y
  show (WIND x y)       = "SPELL WIND " ++ show x ++ " " ++ show y
  show (SHIELD e)       = "SPELL SHIELD " ++ show (getId e)
  show (CONTROL e x y)  = "SPELL CONTROL " ++ show (getId e) ++ " " ++ show x ++ " " ++ show y

-------------------------------------------------------------------------------
-- Range
-------------------------------------------------------------------------------
class HasRange s where
  range :: s -> Float

instance HasRange Command where
  range WAIT        = 20000
  range MOVE {}     = 5000 -- 800 + 800
  range WIND {}     = 1280
  range SHIELD {}   = 2200
  range CONTROL {}  = 2200

isWithinRange command source target = distance source target < range command

-------------------------------------------------------------------------------
-- command and hero actions
-------------------------------------------------------------------------------
move = uncurry MOVE

patrol (MyHero e _) = MyHero e . Just . move

-- TODO: move to where monster will be, no where it is now
attack (MyHero e _) = MyHero e . Just . move . coords . advance

redirect (MyHero e _) t (x, y) = MyHero e (Just $ CONTROL t x y)

getCommand (MyHero _ (Just c)) = c

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------
distance e1 e2 = distance2d (coords e1) (coords e2)

distance2d (x1, y1) (x2, y2) =
  let dx = x1 - x2
      dy = y1 - y2
  in magnitude dx dy

magnitude dx dy = sqrt $ fromIntegral (dx*dx + dy*dy)

isMyHero MyHero {} = True
isMyHero _ = False

isAThreat (Monster _ md) = monsterThreatFor md == MyBase
isAThreat _ = False

isAThreatToEnemy (Monster _ md) = monsterThreatFor md == EnemyBase
isAThreatToEnemy _ = False

isMonster (Monster _ _) = True
isMonster _ = False

defaultPosition i = defaultPositions M.! i
  where
    defaultPositions = M.fromList [ (0, (6400, 2000))
                                  , (1, (4500, 4500))
                                  , (2, (2000, 6400))
                                  , (3, (mapWidth - 5500, mapHeight - 1500))
                                  , (4, (mapWidth - 4000, mapHeight - 4000))
                                  , (5, (mapWidth - 1500, mapHeight - 5500)) ]

-------------------------------------------------------------------------------
-- World
-------------------------------------------------------------------------------
updateWorld (World b hs ms es) as = World b as ms es

simulate :: World -> World
simulate w@(World b hs ms ehs) = World base hs ums ehs
  where
    !ums = updateVector b . advance <$> interactMonstersWithHeroes ms advancedHeroes
    advancedHeroes = fmap  advance hs
    !base = b {baseMana = baseMana b - sum (fmap getSpellMana hs)}

updateVector b m = if distance b m < 5000 then redirectMonster m (coords b) else m

advance :: Entity -> Entity
advance (Monster e m) = Monster (e {entityCoord = addVector (entityCoord e) (monsterV m)}) m
advance (MyHero e c@(Just (MOVE x y))) = MyHero (e {entityCoord = applySpeedToVector (calculateNormalizedDirectionVector (entityCoord e) (x, y)) 800}) c
advance h@(MyHero _ _) = h

calculateNormalizedDirectionVector :: Coords -> Coords -> Coords
calculateNormalizedDirectionVector (!x1, !y1) (!x2, !y2) =
  let
    dx = x2 - x1
    dy = y2 - y1
  in normalizeVector (dx, dy)

normalizeVector :: Coords -> Coords
normalizeVector (!x, !y) =
  let
    mag = magnitude x y
    ndx = round (fromIntegral x/mag)
    ndy = round (fromIntegral y/mag)
  in (ndx, ndy)

applySpeedToVector :: Coords -> Int -> Coords
applySpeedToVector (!x, !y) !s = (x*s, y*s)

addVector :: Coords -> Coords -> Coords
addVector (!x, !y) (!dx, !dy) = (x + dx, y + dy)

getSpellMana (MyHero _ (Just WIND {}))     = 10
getSpellMana (MyHero _ (Just SHIELD {}))   = 10
getSpellMana (MyHero _ (Just CONTROL {}))  = 10
getSpellMana _ = 0

interactMonstersWithHeroes = foldr interactWithMonsters

interactWithMonsters h = fmap (interactWithMonster h)

interactWithMonster !h !m =
  let
    ms = applyControlSpell h m
  in if distance h ms <= 800 then takeAHit ms else ms

applyControlSpell :: Entity -> Entity -> Entity
applyControlSpell h@(MyHero _ (Just (CONTROL e tx ty))) m  = if getId e == getId m then redirectMonster m (tx, ty) else m
  where
applyControlSpell _ m = m

redirectMonster (Monster me md) v =
  Monster
    ( me {entityCoord = applySpeedToVector (calculateNormalizedDirectionVector (entityCoord me) v) 400, entityIsControlled = True})
    ( md {monsterThreatFor = EnemyBase})


takeAHit (Monster e m) = Monster e m {monsterHealth = monsterHealth m - 2}

-------------------------------------------------------------------------------
-- main logic
-------------------------------------------------------------------------------
filterMyHeroes :: [Entity] -> [Entity]
filterMyHeroes = filter isMyHero

filterMonsters :: [Entity] -> [Entity]
filterMonsters = filter isMonster

-- ✓ simplest logic - for each hero find the closes monster who's aiming own base and move to it
-- improvement: separate monster for each hero
-- ✓ improvement: do not go far from own base
-- patrol on a border of intersection of base's and hero's visions
-- redirect threat monsters to enemy base when spotted
-- attack monsters when they're in a patrolling area

defaultAction :: p -> Entity -> Entity
defaultAction b h = patrol h $ defaultPosition (getId h)

offensiveActionableActions :: Base -> Entity -> Entity -> [Entity]
offensiveActionableActions b h m = filter (isActionable b) $ offensiveActions b h m

offensiveActions :: Base -> Entity -> Entity -> [Entity]
offensiveActions b h m =
  [ attack h m
  , redirect h m (enemyBaseCoords b)
  ]

heroActions b h ms = filter (isActionable b) (ams ++ [da])
  where
    !ams = concatMap (offensiveActionableActions b h) ms
    !da = defaultAction b h

generateActions (World base myHeroes monsters _) = fmap (\h -> heroActions base h monsters) myHeroes

isActionable :: Base -> Entity -> Bool
isActionable b (MyHero _ Nothing)                     = False
isActionable b (MyHero e (Just c@(MOVE x y)))         = True
isActionable b h@(MyHero ed (Just c@(CONTROL e x y))) = (baseMana b > 10) && isWithinRange c h e

generateAllActions w = sequence $ generateActions w
--generateNewWorlds !w = trace ("generateNewWorlds (a: " ++ show (length actions) ++ ")") map (simulate . updateWorld w) actions
generateNewWorlds !w = map (simulate . updateWorld w) actions
  where
    !actions = generateAllActions w

-------------------------------------------------------------------------------
-- score
-------------------------------------------------------------------------------
-- score priority:
-- enemy on a base
-- distance from monster closest to base
-- distance from default position
-- cast control
-- attack monster

class HasScore s where
  score :: s -> Float

instance HasScore Entity where
  score m@(Monster e md)  =
    let
      mh = monsterHealth md
      threatMult = if isAThreat m then 3.0 else 1.0
      nearBaseMult = if monsterNearBase md then 5.0 else 1.0
      threatToEnemy = if isAThreatToEnemy m then 0.1 else 1.0
    in fromIntegral mh * threatMult * nearBaseMult

  score (MyHero    e _)   = 100
  score (EnemyHero e)     = 1

instance HasScore Base where
  score (Base _ h m) = let baseScore = (if m < 0 then 1000 else h)
    in fromIntegral baseScore - (fromIntegral m/5)

calculateMonsterScore b m = score m + linearDistanceFactor 2000 (baseCoords b) m

distanceFactor d t e = exp (1 - (d - distance2d t (coords e)) / d)
linearDistanceFactor d t e = abs (distance2d t (coords e)) / d
calculateHeroeScore b ms h = score h * monsterDistanceFactor
  where
    baseDistanceFactor = linearDistanceFactor 1000 (defaultPosition (getId h)) h
    monsterDistanceFactor = 10 * calculateDistanceFromHeroToTargetMonsters (coords $ V.head ms) h
calculateDistanceFromHeroToTargetMonsters = linearDistanceFactor 400

instance HasScore World where
  score (World b hs ms es) = score b + sum (fmap (calculateHeroeScore b ms) hs) + sum (fmap (calculateMonsterScore b) ms)

-------------------------------------------------------------------------------
-- forecast
-------------------------------------------------------------------------------
type ForecastTree = Tree World

forecast :: Int -> Int -> World -> World
forecast depth width !ws = pickBestCandidate $ forecastWorlds depth width ws

pickBestCandidate :: ForecastTree -> World
pickBestCandidate (Node w !ws) = snd $ head $ sortNodesByTreeScore $ map (\w -> (calculateTreeScore w, rootLabel w)) ws
  where
    !sortNodesByTreeScore = sortBy (\(s1, w1) (s2, w2) -> if s1 < s2 then LT else GT)

-- TODO: possible optimization is to add some coeff for "future world's score" like *0.9 or so
calculateTreeScore :: ForecastTree -> Float
calculateTreeScore (Node w []) = score w
--calculateTreeScore (Node w ws) = score w + sum (map calculateTreeScore ws)
calculateTreeScore (Node w ws) = minimum (map calculateTreeScore ws)

forecastWorlds :: Int -> Int -> World -> ForecastTree
forecastWorlds 0 _ w = Node w []
forecastWorlds depth width !world = Node world (map (forecastWorlds (depth - 1) width) worlds)
  where
    -- !worlds = trace ("scores : " ++ show (map (show . score) sorted)) take width sorted
    !worlds = take width sorted
    !sorted = sortByScore $ generateNewWorlds world

sortByScore :: [World] -> [World]
sortByScore = sortBy (\w1 w2 -> if score w1 < score w2 then LT else GT)

-------------------------------------------------------------------------------
-- game loop and main logic
-------------------------------------------------------------------------------
-- ✓ simplest logic - for each hero find the closes monster who's aiming own base and move to it
-- improvement: separate monster for each hero
-- ✓ improvement: do not go far from own base
-- patrol on a border of intersection of base's and hero's visions
-- redirect threat monsters to enemy base when spotted
-- attack monsters when they're in a patrolling area

simplifyWorld :: Int -> World -> World
simplifyWorld targets (World b hs ms ehs) = World b hs highThreatMonsters ehs
  where
    highThreatMonsters = V.fromList $ take targets $ sortBy (\m1 m2 -> if distance b m1 < distance b m2 then LT else GT) (V.toList ms)

gameLoop :: World -> IO World -> IO World
gameLoop prevWorld newWorld = do
  worldBig <- newWorld
  let world = simplifyWorld 4 worldBig
  debug world

  let newWorld = forecast 2 30 world
  -- debug world
  -- debug $ score newWorld

  -- debug $ show $ head allCombinations
  mapM_ (\h -> putStrLn $ show $ getCommand h) (worldMyHeroes newWorld)

  return newWorld

-------------------------------------------------------------------------------
-- read state helper functions and main
-------------------------------------------------------------------------------
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE
  hSetBuffering stderr NoBuffering
  base <- makeBase <$> readInts
  debug $ show base
  heroesPerPlayer <- readInt -- ignored
  let getReadWorld = readWorld base
  world <- getReadWorld ()

  foldM_ gameLoop world (pure world : repeat (getReadWorld ()))

readInt :: IO Int
readInt = readLn

readInts :: IO [Int]
readInts = map read . words <$> getLine

readEntities :: IO [Entity]
readEntities = do
  entityCount <- readInt
  replicateM entityCount (makeEntity <$> readInts)

readBaseStats b = do
  [health, mana] <- readInts
  pure $ updateBase b health mana

makeEntity :: [Int] -> Entity
makeEntity [id_, type_, x, y, shieldLife, isControlled, health, vx, vy, nearBase, threatFor] =
    let
        properties = EntityData id_ (x, y) shieldLife (isControlled == 1)
        monster = MonsterData health (vx, vy) (nearBase == 1) (toEnum threatFor)
    in
    case type_ of
        0 -> Monster properties monster
        1 -> MyHero properties Nothing
        2 -> EnemyHero properties

makeBase :: [Int] -> Base
makeBase [baseX_, baseY_] = Base (baseX_, baseY_) 3 0

readWorld b () = do
  base <- readBaseStats b
  [enemyHealth, enemyMana] <- readInts
  entities <- readEntities
  let myHeros = V.fromList $ filterMyHeroes entities
  let monsters = V.fromList $ filterMonsters entities
  return $ World base myHeros monsters []

worldFromList b hs ms = World b (V.fromList hs) (V.fromList ms)