{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Ship
  ( initGame
  , initMenu
  , step
  , turn
  , decrementTimer
  , _rockGenerator
  , Game(..)
  , Direction(..)
  , dead, rocks, score, ship, time, endState
  , endGame
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
  , _rockGenerator  :: [(V2 Int)] -- ^ infinite list of random next food locations
  , _dead   :: Bool         -- ^ game over flag
  , _score  :: Int          -- ^ score
  , _time   :: Int
  , _ticksElapsed :: Int
  , _speedFactor :: Int
  , _endState :: Int
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

nextRocks :: Game -> Game
nextRocks g@Game { _rockGenerator = r:rs } = updateRockState (g & rockGenerator .~ rs) (rockProducer r)

-- nextRocks :: State Game ()
-- nextRocks = do
--   rg <- use rockGenerator
--   let f = head (rg)
--   let fs = tail (rg)
--   currGame <- get
--   put (currGame & rockGenerator .~ fs)
--   -- put (currGame)
--   currGameState <- get
--   put (updateRockState currGameState $ rockProducer f)
--   return ()

isRockEnd :: Rock -> Bool
isRockEnd ((0, _), 1) = True
isRockEnd ((x, _), 0)
  | x == width-1      = True
  | otherwise         = False
isRockEnd _           = False

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
moveSpace g@Game { _rocks = rrs } = g & rocks .~ (killRocks $ moveRocks rrs)

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
hasCollidedRocks _ []       = False

addRocksAtRandom :: Game -> Game
addRocksAtRandom g@Game { _rockGenerator = (V2 0 _):rs }  = g & rockGenerator .~ rs
addRocksAtRandom g@Game { _rockGenerator = _:rs }         = nextRocks $ g & rockGenerator .~ rs
-- addRocksAtRandom :: State Game ()
-- addRocksAtRandom = do
--     rg <- use rockGenerator
--     let f   = head (rg)
--     let fs  = tail (rg)
--     currGame <- get
--     put (currGame & rockGenerator .~ fs)
--     -- put (currGame)
--     case f of
--       (V2 0 _) -> return ()
--       _        -> nextRocks

resetShip :: Game -> Game
resetShip g = g & ship .~ startingCoords

-- pauseShip 
die :: Game -> Game
die g@Game {_ship = s, _rocks = rss, _dead = d}
  | d                       = g
  | hasCollidedRocks s rss  = updateSpeed (-1) $ updateScore (-1) $ resetShip g
  | otherwise               = g

updateSpace :: Game -> Game
updateSpace g@Game { _rocks = rrs, _ticksElapsed = te, _speedFactor = sp }
  | (te `mod` sp) == 0  = addRocksAtRandom $ moveSpace g --execState addRocksAtRandom $ moveSpace g
  | otherwise           = g

step :: Game -> Game
step g = updateSpace $ die $ incrementTicksElapsed g

incrementTicksElapsed :: Game -> Game
incrementTicksElapsed g@Game { _ticksElapsed = te } = g & ticksElapsed .~ (te + 1)

directionStep :: Direction -> Coord -> Coord
directionStep d (x,y)
  | d == North  = (x, (y + 1) `mod` height)
  | d == South  = (x, (y - 1) `mod` height)
  | d == East   = ((x + 1) `mod` width, y)
  | otherwise   = ((x - 1) `mod` width, y)

moveShip :: Direction -> Ship -> Ship
moveShip d = map (directionStep d)

turn :: Direction -> Game -> Game
turn _ g@Game { _dead = True} = g
turn d g@Game { _ship = s}     --checkIfTop $ g & ship .~ moveShip d s
  | checkIfTopB s' = updateSpeed 1 $ updateScore 1 $ resetShip g'
  | otherwise     = g'
  where
    g'@Game { _ship = s'} = g & ship .~ moveShip d s

checkIfTopB :: Ship -> Bool
checkIfTopB ((x,y):_)
  | y >= height-1 = True
  | otherwise     = False

-- checkIfTop:: Game -> Game
-- checkIfTop g@Game{_ship = (x,y):xs} = if y >= height-1 then updateSpeed 1 $ updateScore 1 $ resetShip g else g
-- checkIfTop _ = error "Ship can't be empty!"

updateSpeed :: Int -> Game -> Game
updateSpeed v g@Game { _speedFactor = sp } = g & speedFactor .~ (min 8 $ max 2 $ sp-v)

updateScore :: Int -> Game -> Game
updateScore v g@Game { _score = s} = g & score .~ (max 0 $ s + v)

resetScore :: Game -> Game
resetScore g = g & score .~ 0

setGameOver :: Game -> Game
setGameOver g@Game { _time = 0 }  = g & dead .~ True
setGameOver g                     = g

decrementTimer :: Game -> Game
decrementTimer g@Game { _time = t, _dead = d}
  | d           = g
  | otherwise   = setGameOver $ g & time .~ (t - 1)

endGame :: Game -> Int -> Game
endGame g t = g & endState .~ t

initGame :: IO Game
initGame = do
  (f : fs) <-
    randomRs (V2 0 5, V2 2 height-1) <$> newStdGen
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _ship  = startingCoords 
        , _rocks  = []
        , _rockGenerator  = fs
        , _score  = 0
        , _dead   = False
        , _time   = 15
        , _ticksElapsed  = 0
        , _speedFactor   = 8
        }
  return g

initMenu :: IO Game
initMenu = do
  let
      g  = Game {
        _endState = 0
      }
  return g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")