module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

window :: Display
window = InWindow "Jump Dash" (1000, 600) (10, 10)

background :: Color
background = white

type Position = (Float, Float)

data GameState = GameState
  { playerPos :: Position
  , playerVel :: Float
  }

initialState :: GameState
initialState = GameState (-350, -15) 40

drawGame :: GameState -> Picture
drawGame gs = pictures
  [ translate x y $ color blue $ rectangleSolid 70 70
  , translate 0 (-200) $ color (dark (dark blue)) $ rectangleSolid 1000 300
  ]
  where (x, y) = playerPos gs

handleInput :: Event -> GameState -> GameState
handleInput _ gs = gs


main :: IO ()
main = play window background 60 initialState drawGame handleInput updateGame

{-
main :: IO ()
main = display window background objectsDrawing

player ::  Picture
player = translate (-350) (-15) $ color blue $ rectangleSolid 70 70

ground :: Picture
ground = translate 0 (-200) $ color (dark (dark blue)) $ rectangleSolid 1000 300

-}