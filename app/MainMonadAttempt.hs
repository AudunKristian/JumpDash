module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.Trans.State
import Control.Monad 


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

type GameMonad = StateT GameState IO

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
drawGame state = pictures
  [ translate x y $
      rotate (playerRotation state) $
      drawPlayer
  , translate 0 (-200) $ drawGround
  , pictures $ map drawObstacle (obstacles state)
  ]
  where (x, y) = playerPos state

handleEvent :: Event -> GameMonad ()
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) = do
  state <- get 
  when (not (isJumping state)) $ do 
    put state { isJumping = True, playerVelocityY = 1000, playerRotationVelocity = -1500}
handleEvent _ = return ()

--updateGame :: Float -> GameState -> GameState
updateGame :: Float -> GameMonad ()
updateGame dt = do 
  state <- get
  let (x, y) = playerPos state
      vy     = playerVelocityY state
      vy'    = vy - 3000 * dt  -- Gravity
      y'     = max (-15) (y + vy * dt)

      isJumping' = if y' == -15 then False else isJumping state

      p      = if isJumping' then playerRotation state else 0
      vp     = playerRotationVelocity state
      vp'    = if isJumping' then vp * 0.95 else 0
      p'     = p + vp * dt

      obstacles' = fmap (\(z, w) -> ((z - (70 * dt)), w)) (obstacles state)
      
  put state { playerPos = (x, y'), playerVelocityY = vy', playerRotation = p', playerRotationVelocity = vp',  isJumping = isJumping', obstacles = obstacles'}

main :: IO ()
main = do
  let initialState' = initialState
  play window background 60 initialState' drawGame handleEvent updateGame


{-
main :: IO ()
main = display window background objectsDrawing

player ::  Picture
player = translate (-350) (-15) $ color blue $ rectangleSolid 70 70

ground :: Picture
ground = translate 0 (-200) $ color (dark (dark blue)) $ rectangleSolid 1000 300

-}