module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Position = (Float, Float)

data GameState = GameState
  { playerPos :: Position,
    playerVelocityY :: Float,
    isJumping :: Bool,
    playerRotation  :: Float,  
    playerRotationVelocity :: Float,
    obstacles :: [Position],
    surfacesY :: [Float]
  }

--type GameMonad = StateT GameState IO

window :: Display
window = InWindow "Jump Dash" (1000, 600) (10, 10)

background :: Color
background = white

addBorder :: Float -> Float -> Picture
addBorder x y = color black $ rectangleWire x y

initialObstacles :: [Position]
initialObstacles =
  [ (0, -15)
  , (200, 80)
  , (600, 55)
  , (900, -15)
  , (1200, 100)
  , (1500, 30)
  , (1800, -15)
  , (2100, 70)
  , (2400, -15)
  , (2700, 90)
  , (3000, 40)
  , (3300, -10)
  , (3600, 60)
  , (3900, 20)
  ]


drawPlayer :: Picture
drawPlayer = pictures
  [ 
    color blue $ rectangleSolid 70 70,
    addBorder 70 70
  ]
  
drawGround :: Picture
drawGround = pictures 
  [
    color (dark (dark blue)) $ rectangleSolid 1000 300,
    addBorder 1000 300
  ]

drawObstacle :: Position -> Picture
drawObstacle (x, y) = translate x y $ pictures
  [ 
    color red $ rectangleSolid 70 70,
    addBorder 70 70
  ]

initialState :: GameState
initialState = GameState (-350, -15) 0 False 0 0 initialObstacles []

drawGame :: GameState -> Picture
drawGame gs = pictures
  [ translate x y $
      rotate (playerRotation gs) $
      drawPlayer
  , translate 0 (-200) $ drawGround
  , pictures $ map drawObstacle (obstacles gs)
  ]
  where (x, y) = playerPos gs

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state | not (isJumping state) = state { isJumping = True, playerVelocityY = 1000, playerRotationVelocity = -1500}
handleEvent _ state = state

updateGame :: Float -> GameState -> GameState
updateGame dt gs = gs { playerPos = (x, y'), playerVelocityY = vy', playerRotation = p', playerRotationVelocity = vp',  isJumping = isJumping', obstacles = obstacles'}

  where
    (x, y) = playerPos gs
    vy     = playerVelocityY gs
    vy'    = vy - 3000 * dt  -- Gravity
    y'     = max (-15) (y + vy * dt)

    isJumping' = if y' == -15 then False else isJumping gs

    p      = if isJumping' then playerRotation gs else 0
    vp     = playerRotationVelocity gs
    vp'    = if isJumping' then vp * 0.95 else 0
    p'     = p + vp * dt

    obstacles' = fmap (\(z, w) -> ((z - (70 * dt)), w)) (obstacles gs)


    

main :: IO ()
main = play window background 60 initialState drawGame handleEvent updateGame

{-
main :: IO ()
main = display window background objectsDrawing

player ::  Picture
player = translate (-350) (-15) $ color blue $ rectangleSolid 70 70

ground :: Picture
ground = translate 0 (-200) $ color (dark (dark blue)) $ rectangleSolid 1000 300

-}