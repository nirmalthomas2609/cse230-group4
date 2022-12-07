{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import Ship ( Direction(West, North, South, East),
      Game,
      Coord,
      dead,
      rocks,
      score,
      time,
      ship,
      endState,
      isRockEnd,
      height,
      width,
      step,
      turn,
      decrementTimer,
      initGame,
      initMenu,
      endGame )

main :: IO ()
main = runTests
    [ testIsRockEnd ]

testIsRockEnd ::  Score -> TestTree
testIsRockEnd sc = testGroup "List" [
  scoreTest ((\_ -> isRockEnd ((0,4), 1)), (), True, 1, "t-1"),
  scoreTest ((\_ -> isRockEnd ((3,6), 1)), (), False, 1, "t-2"),
  scoreTest ((\_ -> isRockEnd ((44,4), 0)), (), True, 1, "t-3")
    ]
    where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
        scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
