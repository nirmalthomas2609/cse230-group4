{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
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
    [ testIsRockEnd ]

testIsRockEnd ::  Score -> TestTree
testIsRockEnd sc = testGroup "List" [
  scoreTest ((\_ -> isRockEnd ((0,4), 1)), (), True, 1, "rockEndCheck1"),
  scoreTest ((\_ -> isRockEnd ((3,6), 1)), (), False, 1, "rockEndCheck2"),
  scoreTest ((\_ -> isRockEnd ((44,4), 0)), (), True, 1, "rockEndCheck3"),
  scoreTest ((\_ -> _score (resetScore (Game {_score = 9}))), (), 0, 1, "resetScore4")
    ]
    where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
        scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)