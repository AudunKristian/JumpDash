module Main (main) where

import Graphics.Gloss

window :: Display
window = InWindow "Jump Dash" (800, 600) (10, 10)
background :: Color
background = white

player :: Picture
player = translate (-300) (-40) $
         Pictures
           [ color blue $ polygon playerShape
           , color black $ lineLoop playerShape
           ]
  where
    playerShape = [(0,0), (0,70), (70,70), (70,0)]

ground :: Picture
ground = translate (-400) (-300) $
         Pictures
           [ color (dark (dark blue)) $ polygon groundShape
           , color black $ lineLoop groundShape
           ]
  where
    groundShape = [(0,0), (0,260), (800,260), (800,0)]

main :: IO ()
main = display window background (Pictures [ground, player])
