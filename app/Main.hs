module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

window :: Display
window = InWindow "Jump Dash" (800, 600) (10, 10)

background :: Color
background = white

playerShape :: [(Float, Float)]
playerShape = [(0,0),(0,70),(70,70),(70,0)]

groundShape :: [(Float, Float)]
groundShape = [(0,0),(0,260),(800,260),(800,0)]

data Obstacle = Obstacle Float Float Float Float

data GameState = GameState
  { playerPos     :: (Float, Float)
  , isJumping     :: Bool
  , velocityY     :: Float
  , rotation      :: Float
  , angVel        :: Float
  , jumpHeight    :: Float
  , obstacles     :: [Obstacle]
  , obstacleTimer :: Float
  }

initialState :: GameState
initialState = GameState
  { playerPos     = (-300, -90)
  , isJumping     = False
  , velocityY     = 0
  , rotation      = 0
  , angVel        = 0
  , jumpHeight    = 200
  , obstacles     = []
  , obstacleTimer = 0
  }

draw :: GameState -> Picture
draw state = Pictures
  [ translate (-400) (-350) $ Pictures
      [ color (dark (dark blue)) $ polygon groundShape
      , color black $ lineLoop groundShape
      ]
  , let (px, py) = playerPos state in
      translate px py $
      translate 35 35 $
      rotate (rotation state) $
      translate (-35) (-35) $
      Pictures
        [ color blue $ polygon playerShape
        , color black $ lineLoop playerShape
        ]
  , Pictures $ map drawObstacle (obstacles state)
  ]

drawObstacle :: Obstacle -> Picture
drawObstacle (Obstacle x y w h) =
  translate x y $
    Pictures
      [ color red $ polygon [(0,0),(0,h),(w,h),(w,0)]
      , color black $ lineLoop [(0,0),(0,h),(w,h),(w,0)]
      ]

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state
  | not (isJumping state) =
      state { isJumping = True, velocityY = 20, angVel = -30 }
handleEvent _ state = state

update :: Float -> GameState -> GameState
update dt state =
  let newState = if isJumping state
                 then let (px, py) = playerPos state
                          newY = py + velocityY state
                          newVelocityY = velocityY state - 1
                          newRotation = rotation state + angVel state
                          newAngVel = angVel state * 0.95
                      in if newY <= -90
                         then state { isJumping = False, velocityY = 0, rotation = 0, angVel = 0, playerPos = (px, -90) }
                         else state { velocityY = newVelocityY, playerPos = (px, newY), rotation = newRotation, angVel = newAngVel }
                 else state
      speed = 500
      movedObs = map (\(Obstacle x y w h) -> Obstacle (x - speed * dt) y w h) (obstacles newState)
      filteredObs = filter (\(Obstacle x _ w _) -> x + w > -400) movedObs
      newTimer = obstacleTimer newState + dt
      threshold = 2
      (finalObs, finalTimer) = if newTimer >= threshold
                               then (filteredObs ++ [Obstacle 400 (-90) 70 70], 0)
                               else (filteredObs, newTimer)
  in newState { obstacles = finalObs, obstacleTimer = finalTimer }

main :: IO ()
main = play window background 60 initialState draw handleEvent update
