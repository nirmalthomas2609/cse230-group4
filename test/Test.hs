{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Test.QuickCheck

import Common
import Ship ( Direction(West, North, South, East),
      Game(..),
      Coord,
      dead,
      rocks,
      score,
      time,
      ship,
      endState,
      isRockEnd,
      resetScore,
      hasCollided,
      height,
      width,
      step,
      turn,
      decrementTimer,
      initGame,
      initMenu,
      _score,
      endGame,
      updateScore,
      killRocks )


main :: IO ()
main = runTests
    [ testIsRockEnd,
      testHasCollided,
      propTests
    ]

testIsRockEnd ::  Score -> TestTree
testIsRockEnd sc = testGroup "List" [
  scoreTest ((\_ -> isRockEnd ((0,4), 1)), (), True, 1, "rockEndCheck1"),
  scoreTest ((\_ -> isRockEnd ((3,6), 1)), (), False, 1, "rockEndCheck2"),
  scoreTest ((\_ -> isRockEnd ((44,4), 0)), (), True, 1, "rockEndCheck3"),
  scoreTest ((\_ -> _score (resetScore (Game {_score = 9}))), (), 0, 1, "resetScore4"),
  scoreTest ((\_ -> length (killRocks [((45, 3), 0), ((46, 0), 0), ((0, 0), 1)])), (), 0, 1, "killRocks1")
    ]
    where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
        scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

propIsRockEndFalse :: (Int, Int) -> Property
propIsRockEndFalse p@(x,y) = 
    not (x<=0) ==> isRockEnd (p, 1) == False

propIsRockEndTrue :: Int -> Property
propIsRockEndTrue y = 
    (y>0 || y<=0) ==> isRockEnd ((0,y), 1) == True

propIsRockEndTrue2 :: Int -> Property
propIsRockEndTrue2 y = 
    (y>0 || y<=0) ==> isRockEnd ((45,y), 0) == True

propUpdateScore :: (Int, Int) -> Property
propUpdateScore (x, y) = 
    (x >= 0 && y >= 0) ==> _score (updateScore x (Game {_score = y})) == x + y

-- propKillRocks :: Int -> Property
-- propKillRocks x = 
--     (x == 45) ==> length (killRocks [((x, 3), 0)]) == 0

-- propHasCollided :: (Int,Int) -> Property
-- propHasCollided (x, y) = 
--     (x>=0 && y>=0) ==> 

propTests :: Score -> TestTree
propTests sc = testGroup "Property Based Tests" [ 
    scoreProp sc ("prop_is_rock_end_false", propIsRockEndFalse, 1),
    scoreProp sc ("prop_update_score", propUpdateScore, 1),
    scoreProp sc ("prop_is_rock_end_true", propIsRockEndTrue, 1),
    scoreProp sc ("prop_is_rock_end_true2", propIsRockEndTrue2, 1)
    ]

testHasCollided :: Score -> TestTree
testHasCollided sc = testGroup "HasCollided" [
    scoreTest ((\_ -> hasCollided [(0,4),(-1, 4),(-1, 5)] ((-1, 5), 1)), (), True, 1, "Collided-1"),
    scoreTest ((\_ -> hasCollided [(0,4),(-1, 4),(-1, 5),(0, 5)] ((0, 6), 1)), (), False, 1, "Collided-2")
    ]
    where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
        scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)