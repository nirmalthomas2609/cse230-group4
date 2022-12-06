{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Ship
    ( Direction(West, North, South, East),
      Game,
      Coord,
      dead,
      rocks,
      score,
      time,
      ship,
      height,
      width,
      step,
      turn,
      decrementTimer,
      initGame,
      initMenu )

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick | Timer | SpeedUp

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Ship | Rock | Empty

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

appM :: App Game Tick Name
appM = App { appDraw = drawUIMenu
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEventMenu
          , appStartEvent = return
          , appAttrMap = const theMap
          }



-- createTickThread :: Int -> BChan a -> IO ThreadId
-- createTickThread delay chan = forkIO $ forever $ do { writeBChan chan Tick; threadDelay delay;}                                               

-- deployTickThread :: IO ThreadId -> IO ()
-- deployTickThread tickThread = do

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Timer
    threadDelay 1000000 -- decides how fast your game moves

  tickID <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 10000 -- decides how fast your game moves

  m <- initMenu
  let builderMenu = V.mkVty V.defaultConfig
  initialVtyMenu <- builderMenu
  
  void $ customMain initialVtyMenu builderMenu (Just chan) appM m

  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  g <- initGame
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEventMenu :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEventMenu g (VtyEvent (V.EvKey (V.KChar 's') [])) = halt g
handleEventMenu g _ = continue g

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (AppEvent Timer)                      = continue $ decrementTimer g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g 
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- Drawing

drawUIMenu :: Game -> [Widget Name]
drawUIMenu _ = [ C.center $ padRight (Pad 2) drawMenu ]

drawUI :: Game -> [Widget Name]
drawUI g = [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]
-- drawUI g = [ C.center $ padRight (Pad 2) drawMenu ]

drawMenu :: Widget Name
drawMenu = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "SPACEFORCE")
  $ C.hCenter
  $ padAll 1
  $ str "Press s to start \nInstructions:\n1. Use arrows to move the spaceship. Avoid the obstacles and reach the end to score a point.\n2. If you hit an obstacle, the score is reduced by one point.\n3. The game speeds up as you score more points and slows down as the score goes down.\n4. The objective is to get as many points as possible before the timer hits 0."
  -- $ C.hLeft
  -- $ padAll 1
  -- $ str "SPACEFORCE"

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , drawTimer (g ^. time)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawTimer :: Int -> Widget Name
drawTimer n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "imer")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "SPACEFORCE")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (x, y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. ship = Ship
      | (c, 1) `elem` (g ^. rocks) = Rock
      | (c, 0) `elem` (g ^. rocks) = Rock
      | otherwise           = Empty

drawCell :: Cell -> Widget Name
drawCell Ship = withAttr snakeAttr cw
drawCell Rock  = withAttr rockAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr, V.yellow `on` V.yellow)
  , (rockAttr, V.white `on` V.white)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  , (emptyAttr, V.black `on` V.black)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

snakeAttr, rockAttr, emptyAttr :: AttrName
snakeAttr = "snakeAttr"
rockAttr = "rockAttr"
emptyAttr = "emptyAttr"
