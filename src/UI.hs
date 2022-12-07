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
      endState,
      height,
      width,
      step,
      turn,
      decrementTimer,
      initGame,
      initMenu,
      endGame )

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

data Tick = Tick | Timer | SpeedUp

type Name = ()

data Cell = Ship | Rock | Empty


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

main :: IO ()
main = do
  chan <- newBChan 100
  forkIO $ forever $ do
    writeBChan chan Timer
    threadDelay 1000000 

  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 20 

  m <- initMenu
  let builderMenu = V.mkVty V.defaultConfig
  initialVtyMenu <- builderMenu
  
  m <- customMain initialVtyMenu builderMenu (Just chan) appM m

  if((m ^. endState) == 0)
    then do
      let builder = V.mkVty V.defaultConfig
      initialVty <- builder
      g <- initGame
      void $ customMain initialVty builder (Just chan) app g
    else
      return ()

handleEventMenu :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEventMenu g (VtyEvent (V.EvKey (V.KChar 's') [])) = halt (endGame g 0)
handleEventMenu g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt (endGame g 1)
handleEventMenu g _ = continue g

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (AppEvent Timer)                      = continue $ decrementTimer g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g 
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g


drawUIMenu :: Game -> [Widget Name]
drawUIMenu _ = [ C.center $ padRight (Pad 2) drawMenu ]

drawUI :: Game -> [Widget Name]
drawUI g = [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawMenu :: Widget Name
drawMenu = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "SPACEFORCE")
  $ C.hCenter
  $ padAll 1
  $ str "Press \n    s - Start \n    q - Quit \nInstructions:\n1. Use arrows to move the spaceship. Avoid the obstacles and reach the end to score a point.\n2. If you hit an obstacle, the score is reduced by one point.\n3. The game speeds up as you score more points and slows down as the score goes down.\n4. The objective is to get as many points as possible before the timer hits 0."
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
drawScore n = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawTimer :: Int -> Widget Name
drawTimer n = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "Timer")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeRounded
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
drawCell Ship = withAttr shipAttr cw
drawCell Rock  = withAttr rockAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (shipAttr, V.yellow `on` V.yellow)
  , (rockAttr, V.white `on` V.white)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  , (emptyAttr, V.black `on` V.black)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

shipAttr, rockAttr, emptyAttr :: AttrName
shipAttr = "shipAttr"
rockAttr = "rockAttr"
emptyAttr = "emptyAttr"
