module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

window :: Display
window = InWindow "Jump Dash" (800, 600) (10, 10)

background :: Color
background = white

player ::  Picture
player = color blue $ rectangleSolid 70 70

ground :: Picture
ground = translate 0 (-230) $ color green $ rectangleSolid 300 400

objectsDrawing :: Picture
objectsDrawing = pictures [player, ground]

main :: IO ()
main = display window background objectsDrawing

