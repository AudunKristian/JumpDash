module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Position = (Float, Float)

data GameStatus = MainMenu | Running | Paused | GameOver | Controls
  deriving (Eq, Show)

data GameState = GameState
  { playerPos :: Position,
    playerVelocityY :: Float,
    isJumping :: Bool,
    playerRotation  :: Float,  
    playerRotationVelocity :: Float,
    obstacles :: [Position],
    surfacesY :: [Float],
    gameStatus :: GameStatus
  }

--type GameMonad = gsT GameState IO

window :: Display
window = InWindow "Jump Dash" (1000, 600) (10, 10)

background :: Color
background = white

addBorder :: Float -> Float -> Picture
addBorder x y = color black $ rectangleWire x y

initialObstacles :: [Position]
initialObstacles =
  [ 
    (600, 80)
  , (900, -15)
  , (1200, 100) 
  , (1500, 40)
  , (1800, -15)
  , (2100, 70)
  , (2400, -15)
  , (2700, 90)
  , (3000, 40)
  , (3300, -15)
  , (3600, 60)
  , (3900, -15)
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

drawGameOverScreen :: Picture
drawGameOverScreen = pictures [ drawColorOverlay, 
                                (drawText ((-180), 20) 0.5 "Game Over"), 
                                (drawText ((-250), (-100)) 0.2 "Press [Space] to try again"),
                                (drawText ((-250), (-160)) 0.2 "Press [Backspace] for main menu")
                              ]

drawMainMenuScreen :: Picture
drawMainMenuScreen = pictures [ drawColorOverlay, 
                                (drawText ((-180), (20)) 0.5 "Jump Dash"), 
                                (drawText ((-250), (-100)) 0.2 "Press [Space] to start"),
                                (drawText ((-250), (-160)) 0.2 "Press [C] for controls")
                              ]

drawControlsScreen :: Picture
drawControlsScreen = pictures [ drawColorOverlay, 
                                (drawText ((-180), (-40)) 0.5 "Jump Dash"), 
                                (drawText ((-180), (-40)) 0.2 "Press [Enter] to start!")
                              ]

drawPausedScreen :: Picture
drawPausedScreen = pictures [ drawColorOverlay, 
                                (drawText ((-180), (-40)) 0.5 "Jump Dash"), 
                                (drawText ((-180), (-40)) 0.2 "Press [Enter] to start!")
                              ]

drawColorOverlay :: Picture 
drawColorOverlay = color (makeColor 0.5 0.5 0.5 0.9) $ rectangleSolid 1000 600

drawText :: Position -> Float -> String -> Picture
drawText (x, y) scaleFactor inputText = translate (x) (y) $ scale scaleFactor scaleFactor $ color white $ text inputText

getTextScreen :: GameStatus -> Picture
getTextScreen MainMenu = drawMainMenuScreen
getTextScreen GameOver = drawGameOverScreen
getTextScreen Controls = drawControlsScreen
getTextScreen Paused   = drawPausedScreen
getTextScreen _  = blank

initialState :: GameStatus -> GameState
initialState gameStatus = GameState (-350, -15) 0 False 0 0 initialObstacles [] gameStatus

drawGame :: GameState -> Picture
drawGame gs = pictures $
  [ translate x y $
      rotate (playerRotation gs) $
      drawPlayer
  , translate 0 (-200) $ drawGround
  , pictures $ map drawObstacle (obstacles gs)
  ] ++ [getTextScreen (gameStatus gs)]

  where (x, y) = playerPos gs

colidesWithPlayer :: Position -> Position -> Bool
colidesWithPlayer (x1, y1) (x2, y2) = 
  let dx = abs (x1 - x2)
      dy = abs (y1 - y2)
  in dx <= 70 && dy <= 70


handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeyBackspace) Down _ _) gs  | gameStatus gs == GameOver = (initialState MainMenu)
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) gs      | gameStatus gs `elem` [Paused, MainMenu, GameOver] = (initialState Running)
                                                              | not (isJumping gs) = gs { isJumping = True, playerVelocityY = 1000, playerRotationVelocity = -1500} 
handleEvent _ gs = gs

updateGame :: Float -> GameState -> GameState
updateGame dt gs 
 | gameStatus gs /= Running = gs
 | otherwise = gs { playerPos = (x, y'), playerVelocityY = vy', playerRotation = p', playerRotationVelocity = vp',  isJumping = isJumping', obstacles = obstacles', gameStatus = gameStatus'}
  
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

    obstacles' = fmap (\(z, w) -> ((z - (400 * dt)), w)) (obstacles gs)
    gameStatus' = if any (colidesWithPlayer (x, y')) obstacles' then GameOver else gameStatus gs

main :: IO ()
main = play window background 60 (initialState MainMenu) drawGame handleEvent updateGame

{-
main :: IO ()
main = display window background objectsDrawing

player ::  Picture
player = translate (-350) (-15) $ color blue $ rectangleSolid 70 70

ground :: Picture
ground = translate 0 (-200) $ color (dark (dark blue)) $ rectangleSolid 1000 300

-}