module QuickTests where

import Test.QuickCheck
import Main (colidesWithPlayer, Position)

testSymmetricCollision :: Position -> Position -> Bool
testSymmetricCollision a b = colidesWithPlayer a b == colidesWithPlayer b a 



main :: IO ()
quickCheck testSymmetricCollision