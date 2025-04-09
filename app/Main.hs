module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Window and display setup
window :: Display
window = InWindow "Jump Dash" (800, 600) (10, 10)

background :: Color
background = white

-- Player shape (rectangle)
playerShape :: [(Float, Float)]
playerShape = [(0,0),(0,70),(70,70),(70,0)]

-- Ground shape (rectangle at bottom)
groundShape :: [(Float, Float)]
groundShape = [(0,0),(0,260),(800,260),(800,0)]

-- Obstacle = position x, y, width, height
data Obstacle = Obstacle Float Float Float Float

-- Game state
data GameState = GameState
  { playerPos     :: (Float, Float)
  , isJumping     :: Bool
  , velocityY     :: Float
  , rotation      :: Float
  , angVel        :: Float
  , jumpHeight    :: Float
  , obstacles     :: [Obstacle]
  , obstacleTimer :: Float
  , rngSeed       :: Int
  }

-- Initial game state
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
  , rngSeed       = 0
  }

-- Draw the game world
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

-- Draw an obstacle
drawObstacle :: Obstacle -> Picture
drawObstacle (Obstacle x y w h) =
  translate x y $
    Pictures
      [ color red $ polygon [(0,0),(0,h),(w,h),(w,0)]
      , color black $ lineLoop [(0,0),(0,h),(w,h),(w,0)]
      ]

-- Handle spacebar for jumping
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state
  | not (isJumping state) =
      state { isJumping = True, velocityY = 20, angVel = -30 }
handleEvent _ state = state

-- Game update function
update :: Float -> GameState -> GameState
update dt state =
  let speed = 500

      -- Move obstacles
      movedObs = map (\(Obstacle x y w h) -> Obstacle (x - speed * dt) y w h) (obstacles state)
      filteredObs = filter (\(Obstacle x _ w _) -> x + w > -400) movedObs

      -- Obstacle timer logic
      newTimer = obstacleTimer state + dt
      spawnInterval = 2
  in if newTimer >= spawnInterval
     then
       let (newObstacles, newSeed) = spawnObstacle (rngSeed state)
           allObstacles = filteredObs ++ newObstacles
       in applyPhysics dt state { obstacles = allObstacles, obstacleTimer = 0, rngSeed = newSeed }
     else
       applyPhysics dt state { obstacles = filteredObs, obstacleTimer = newTimer }

-- Spawn both ground and platform obstacle
spawnObstacle :: Int -> ([Obstacle], Int)
spawnObstacle seed =
  let newSeed = seed + 1
      heightOptions = [-90, -30, 30]
      platformY = heightOptions !! (newSeed `mod` length heightOptions)
      groundObs  = Obstacle 400 (-90) 70 70
      airObs     = Obstacle 600 platformY 70 70
  in ([groundObs, airObs], newSeed)

-- Apply physics and collision logic
applyPhysics :: Float -> GameState -> GameState
applyPhysics _ state =
  let (px, py) = playerPos state
      vy       = velocityY state
      newY     = py + vy
      newVy    = if isJumping state then vy - 1 else 0
      newRot   = rotation state + angVel state
      newAng   = angVel state * 0.95

      feetX    = [px, px + 70]
      feetY    = newY
      platform = findLanding (obstacles state) feetX feetY vy
  in case platform of
       Just (Obstacle _ y _ _) ->
         state { playerPos = (px, y + 70), isJumping = False, velocityY = 0, rotation = 0, angVel = 0 }
       Nothing ->
         if newY <= -90
         then state { playerPos = (px, -90), isJumping = False, velocityY = 0, rotation = 0, angVel = 0 }
         else state { playerPos = (px, newY), velocityY = newVy, rotation = newRot, angVel = newAng }

-- Check if player lands on a platform
findLanding :: [Obstacle] -> [Float] -> Float -> Float -> Maybe Obstacle
findLanding obs [x1, x2] playerY vy =
  let isLanding (Obstacle ox oy ow oh) =
        let top = oy + oh
        in vy <= 0 && playerY >= top && playerY + vy <= top &&
           (x1 < ox + ow) && (x2 > ox)
  in case filter isLanding obs of
       (o:_) -> Just o
       []    -> Nothing
findLanding _ _ _ _ = Nothing

-- Launch the game
main :: IO ()
main = play window background 60 initialState draw handleEvent update
