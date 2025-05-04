module QuickTests where

import Test.QuickCheck
import Main (colidesWithPlayer)

-- Tests with QuickCheck

testSymmetricCollision :: Position -> Position -> Bool
testSymmetricCollision a b = colidesWithPlayer a b == colidesWithPlayer b a 

runTests :: IO ()
quickCheck testSymmetricCollision