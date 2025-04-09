module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game 

window :: Display
window = InWindow "Jump Dash" (800, 600) (10, 10)

background :: Color
background = white

playerShape :: [(Float, Float)]
playerShape = [(0, 0), (0, 70), (70, 70), (70, 0)]

data GameState = GameState
  { playerPos :: (Float, Float),  -- (x, y)
    isJumping :: Bool,            -- Whether the player is currently jumping
    velocityY :: Float,           -- Vertical speed (used for gravity effect)
    jumpHeight :: Float           -- Maximum jump height
  }

initialState :: GameState
initialState = GameState { playerPos = (-300, -40), isJumping = False, velocityY = 0, jumpHeight = 200 }

groundShape :: [(Float, Float)]
groundShape = [(0, 0), (0, 260), (800, 260), (800, 0)]

draw :: GameState -> Picture
draw state = Pictures
  [ translate (-400) (-300) $ 
    Pictures
      [ color (dark (dark blue)) $ polygon groundShape
      , color black $ lineLoop groundShape
      ]
  , let (px, py) = playerPos state
    in translate px py $ 
       Pictures
         [ color blue $ polygon playerShape
         , color black $ lineLoop playerShape
         ]
  ]

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state
  | not (isJumping state) = state { isJumping = True, velocityY = 20 } 
  | otherwise = state  
handleEvent _ state = state

-- Update game state (gravity, jump mechanics, etc.)
update :: Float -> GameState -> GameState
update _ state
  | isJumping state =  
      let (px, py) = playerPos state
          newY = py + velocityY state  
          newVelocityY = velocityY state - 1  
      in if newY <= -40 then
           state { isJumping = False, velocityY = 0, playerPos = (px, -40) }
         else
           state { velocityY = newVelocityY, playerPos = (px, newY) }
  | otherwise = state

main :: IO ()
main = play window background 60 initialState draw handleEvent update
