{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Ship
  ( initGame
  , step
  , turn
  , Game(..)
  , Direction(..)
  , dead, rocks, score, ship
  , height, width, Coord
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen, mkStdGen)

data Game = Game
  { _ship  :: Ship        -- ^ snake as a sequence of points in N2
  , _rocks   :: [Rock]        -- ^ location of the food
  , _rockGenerator  :: Stream (V2 Int)-- ^ infinite list of random next food locations
  , _dead   :: Bool         -- ^ game over flag
  , _score  :: Int          -- ^ score
  } deriving (Show)

type Coord = (Int, Int)

type Rock = (Coord, Int)

type Ship = [Coord]

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 45
width = 45

updateRockState :: Game -> Rock -> Game
updateRockState g@Game {_rocks = rrs} s = g & rocks .~ (rrs ++ [s])

nextRocks :: State Game ()
nextRocks = do
  (f :| fs) <- use rockGenerator
  rockGenerator .= fs
  currGameState <- get
  put (updateRockState currGameState (rockProducer f))
  return ()

isRockEnd :: Rock -> Bool
isRockEnd ((0, _), 1)       = True
isRockEnd ((x, _), 0)
  | x == width-1  = True
  | otherwise   = False
isRockEnd _                 = False

killRocks :: [Rock] -> [Rock]
killRocks rrs@(r:rs)
  | isRockEnd r  = killRocks rs
  | otherwise    = rrs
killRocks [] = []

moveRocks :: [Rock] -> [Rock]
moveRocks []                = []
moveRocks (((x, y), 0):rs)  = ((x+1, y), 0):moveRocks rs
moveRocks (((x, y), 1):rs)  = ((x-1, y), 1):moveRocks rs

moveSpace :: Game -> Game
moveSpace g@Game { _rocks = rrs } = g & rocks .~ (killRocks (moveRocks rrs))

startingCoords :: Ship
startingCoords = [(cx,cy),(cx-1,cy-1),(cx-1,cy-2),(cx+1,cy-1),(cx+1,cy-2)]
                  where cx  = width `div` 2
                        cy  = 3

rockProducer :: V2 Int -> Rock
rockProducer (V2 0 x) = ((0, x), 0)
rockProducer (V2 _ x) = ((width - 1, x), 1)

hasCollided :: Ship -> Rock -> Bool
hasCollided (s:ss) (rc, rd) = s==rc || hasCollided ss (rc, rd)
hasCollided [] _            = False

hasCollidedRocks :: Ship -> [Rock] -> Bool
hasCollidedRocks s (r:rs)   = hasCollided s r || hasCollidedRocks s rs
hasCollidedRocks s []       = False

addRocksAtRandom :: State Game ()
addRocksAtRandom = do
    (f :| fs) <- use rockGenerator
    rockGenerator .= fs
    case f of
      (V2 0 _) -> return ()
      _        -> nextRocks

resetShip :: Game -> Game
resetShip g = g & ship .~ startingCoords

die :: Game -> Game
die g@Game {_ship = s, _rocks = rss}
  | hasCollidedRocks s rss  = resetScore (resetShip g)
  | otherwise               = g

-- movesSpace, die, addRocksAtRandom
step :: Game -> Game
step g = execState addRocksAtRandom (moveSpace (die g))
-- step s = flip execState s . runMaybeT $ do
--   s_ = moveSpace s
--   -- Make sure the game isn't paused or over
--   MaybeT $ guard . not <$> orM [use dead]

--   -- die (moved into boundary), eat (moved into food), or move (move into space)
--   die <|> MaybeT (Just <$> modify move)

directionStep :: Direction -> Coord -> Coord
directionStep d (x,y)
  | d == North  = (x, (y + 1) `mod` height)
  | d == South  = (x, (y - 1) `mod` height)
  | d == East   = ((x + 1) `mod` width, y)
  | otherwise   = ((x - 1) `mod` width, y)

moveShip :: Direction -> Ship -> Ship
moveShip d s = map (directionStep d) s

turn :: Direction -> Game -> Game
turn d g@Game { _ship = s } = checkIfTop(g & ship .~ moveShip d s)


checkIfTop:: Game -> Game
checkIfTop g@Game{_ship = (x,y):xs} = if y >= height-1 then updateScore(resetShip g) else g
checkIfTop _ = error "Ship can't be empty!"

updateScore :: Game -> Game
updateScore g@Game { _score = s} = g & score .~ (s + 1)

resetScore :: Game -> Game
resetScore g = g & score .~ 0

initGame :: IO Game
initGame = do
  (f :| fs) <-
    fromList . randomRs (V2 1 5, V2 1 height-1) <$> newStdGen
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _ship  = startingCoords 
        , _rocks  = []
        , _rockGenerator  = fs
        , _score  = 0
        , _dead   = False
        }
  return g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")