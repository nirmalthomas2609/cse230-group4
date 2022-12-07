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
<<<<<<< HEAD
      resetScore,
=======
      hasCollided,
>>>>>>> 4a25b2fc05c7bda70192238031ab30651d9adfc5
      height,
      width,
      step,
      turn,
      decrementTimer,
      initGame,
      initMenu,
      _score,
      endGame )


main :: IO ()
main = runTests
    [ testIsRockEnd,
      testHasCollided,
      probisRockEnd
    ]

testIsRockEnd ::  Score -> TestTree
<<<<<<< HEAD
testIsRockEnd sc = testGroup "List" [
  scoreTest ((\_ -> isRockEnd ((0,4), 1)), (), True, 1, "rockEndCheck1"),
  scoreTest ((\_ -> isRockEnd ((3,6), 1)), (), False, 1, "rockEndCheck2"),
  scoreTest ((\_ -> isRockEnd ((44,4), 0)), (), True, 1, "rockEndCheck3"),
  scoreTest ((\_ -> _score (resetScore (Game {_score = 9}))), (), 0, 1, "resetScore4")
    ]
    where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
=======
testIsRockEnd sc = testGroup "IsRockEnd" [
    scoreTest ((\_ -> isRockEnd ((0,4), 1)), (), True, 1, "t-1"),
    scoreTest ((\_ -> isRockEnd ((3,6), 1)), (), False, 1, "t-2"),
    scoreTest ((\_ -> isRockEnd ((44,4), 0)), (), True, 1, "t-3")
    ]
    where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
        scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

propIsRockEnd :: (Int, Int) -> Property
propIsRockEnd p@(x,y) = 
    not (x<=45 && x>=0) ==> isRockEnd (p, 1) == False

probisRockEnd :: Score -> TestTree
probisRockEnd sc = testGroup "IsRockEnd" [ 
    scoreProp sc ("prop_is_rock_end", propIsRockEnd , 1)
    ]

testHasCollided :: Score -> TestTree
testHasCollided sc = testGroup "HasCollided" [
    scoreTest ((\_ -> hasCollided [(0,4),(-1, 4),(-1, 5)] ((-1,5), 1)), (), True, 1, "C-1"),
    scoreTest ((\_ -> hasCollided [(0,4),(-1, 4),(-1, 5),(0, 5)] ((0,6), 1)), (), False, 1, "C-2")
    ]
    where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
>>>>>>> 4a25b2fc05c7bda70192238031ab30651d9adfc5
        scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)