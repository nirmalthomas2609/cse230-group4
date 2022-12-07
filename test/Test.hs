{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import Ship

main :: IO ()
main = runTests
    [ testIsRockEnd
    ]

testIsRockEnd ::  Score -> TestTree
