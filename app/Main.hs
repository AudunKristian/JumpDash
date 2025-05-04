module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Test.QuickCheck hiding (scale)
import Data.Bifunctor (first, second)


type Position = (Float, Float)

{-| We have 6 different gamestatuses that can tell the game what to draw. 
    They are very central in the logic of the game.
-}
data GameStatus = MainMenu | Running | Paused | GameOver | Controls | GameWon
  deriving (Eq, Show)

-- | The GameState carries all dynamic information related to the game
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

-- | Underneath are different variables used in the game 
window :: Display
window = InWindow "Jump Dash" (1000, 600) (10, 10)

background :: Color
background = white

addBorder :: Float -> Float -> Picture
addBorder x y = color black $ rectangleWire x y

initialObstacles :: [Position]
initialObstacles =
  [ (600, 80),   (900, -15),  (1200, -15), (1500, 40),  (1800, -15)
  , (2100, 70),  (2400, -15), (2700, 90),  (3000, 40),  (3300, -15)
  , (3600, 60),  (3900, -15), (4200, -15), (4500, -15), (4540, -15)
  , (4760, -15), (5070, -15), (5700, -15), (6000, 150), (6000, 220)
  , (6000, 290), (6500, -15), (6500, 55),  (6500, 125), (6500, 195)
  , (7600, 60),  (8000, -15), (8070, -15), (8140, -15), (8600, 55)
  , (8600, 120), (9000, -15), (9000, 155), (9000, 225), (9500, -15)
  , (9500, 55),  (9500, 125), (9800, -15), (9800, 55),  (9800, 125)
  , (10000, 80), (10000, 150), (10000, 220), (10000, 290)
  , (10000, 360), (10000, 430), (10000, 80), (10000, 150)
  , (10000, 220), (10000, 290), (10000, 360), (10000, 430)
  ]

-- | Underneath are different drawn objects
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


{- | Most of the different GameStatuses have their own menu screen
     These are drawn underneath. The only exception is the running gamestatus, 
     which has continously updated game screen.
-}  
drawMainMenuScreen :: Picture
drawMainMenuScreen = pictures [ drawColorOverlay, 
                                (drawText ((-180), (20)) 0.5 "Jump Dash"), 
                                (drawText ((-250), (-100)) 0.2 "Press [Enter] to start"),
                                (drawText ((-250), (-160)) 0.2 "Press [C] for controls"),
                                (drawText ((-250), (-220)) 0.2 "Press [Esc] to quit")
                              ]

drawControlsScreen :: Picture
drawControlsScreen = pictures [ drawColorOverlay, 
                                (drawText ((-180), (20)) 0.5 "Jump Dash"), 
                                (drawText ((-250), (-100)) 0.2 "Press [Space] to (double)jump. Don't hit red boxes."),
                                (drawText ((-250), (-160)) 0.2 "Press [P] to pause the game"),
                                (drawText ((-250), (-220)) 0.2 "Press [M] for main manu")
                              ]

drawPausedScreen :: Picture
drawPausedScreen = pictures [ drawColorOverlay, 
                                (drawText ((-180), (-20)) 0.5 "Paused"), 
                                (drawText ((-180), (-100)) 0.2 "Press [R] to resume game"),
                                (drawText ((-250), (-160)) 0.2 "Press [M] for main menu")
                              ]

drawGameOverScreen :: Picture
drawGameOverScreen = pictures [ drawColorOverlay, 
                                (drawText ((-180), 20) 0.5 "Game Over"), 
                                (drawText ((-250), (-100)) 0.2 "Press [Enter] to try again"),
                                (drawText ((-250), (-160)) 0.2 "Press [M] for main menu")
                              ]

drawGameWonScreen :: Picture
drawGameWonScreen = pictures [ drawColorOverlay, 
                                (drawText ((-180), 20) 0.5 "Game Won!"), 
                                (drawText ((-250), (-100)) 0.2 "Press [Enter] to play again"),
                                (drawText ((-250), (-160)) 0.2 "Press [M] for main menu")
                              ]


drawColorOverlay :: Picture 
drawColorOverlay = color (makeColor 0.5 0.5 0.5 0.9) $ rectangleSolid 1000 600

drawText :: Position -> Float -> String -> Picture
drawText (x, y) scaleFactor inputText = translate (x) (y) $ scale scaleFactor scaleFactor $ color white $ text inputText

{-| A function that connects a drawing to a specific game status -}
getTextScreen :: GameStatus -> Picture
getTextScreen MainMenu = drawMainMenuScreen
getTextScreen GameOver = drawGameOverScreen
getTextScreen Controls = drawControlsScreen
getTextScreen Paused   = drawPausedScreen
getTextScreen GameWon   = drawGameWonScreen
getTextScreen _  = blank

{-| This is the initial state of the game, that can take a gamestatus as argument. 
    It is used for starting a new game. 
-}

initialState :: GameStatus -> GameState
initialState gameStatus = GameState (-350, -15) 0 False 0 0 initialObstacles [] gameStatus

{-| This function will draw the game depending on the GameStatus.
-}
drawGame :: GameState -> Picture
drawGame gs = pictures $
  [ translate x y $
      rotate (playerRotation gs) $
      drawPlayer
  , translate 0 (-200) $ drawGround
  , pictures $ map drawObstacle (obstacles gs)
  ] ++ [getTextScreen (gameStatus gs)]

  where (x, y) = playerPos gs

{-| A helper-function that calculates if players and objects collide. 
    The player is a box with the size 70 x 70, and loses if an object his hit.
-} 
colidesWithPlayer :: Position -> Position -> Bool
colidesWithPlayer (x1, y1) (x2, y2) = 
  let dx = abs (x1 - x2)
      dy = abs (y1 - y2)
  in dx <= 70 && dy <= 70

{-| This functions handles events, and the game's state is updated accordingly. 
    Different keys are possible to press in different gameStatuses of the game, 
    this functions handles this. 
-}
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) gs  | not (isJumping gs) = gs { isJumping = True, playerVelocityY = 1080, playerRotationVelocity = -1500}
handleEvent (EventKey (Char 'm') Down _ _) gs             | gameStatus gs `elem` [GameOver, Paused, Controls, GameWon] = (initialState MainMenu)
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) gs  | gameStatus gs `elem` [MainMenu, GameOver, GameWon] = (initialState Running)
handleEvent (EventKey (Char 'p') Down _ _) gs             | gameStatus gs == Running = gs { gameStatus = Paused}
handleEvent (EventKey (Char 'r') Down _ _) gs             | gameStatus gs == Running = gs { gameStatus = Running}
handleEvent (EventKey (Char 'c') Down _ _) gs             | gameStatus gs == MainMenu = gs { gameStatus = Controls}
handleEvent _ gs = gs


{-| This function updates the game's state frequently when the game is running. 
    All the movements are updated by this function. This function
    takes care of both the player's jumps and the different obstacles.

    When the game is not running nothing happens.
-}
updateGame :: Float -> GameState -> GameState
updateGame dt gs 
 | gameStatus gs /= Running = gs
 | otherwise = gs { playerPos = (x, y'), playerVelocityY = vy', playerRotation = p', playerRotationVelocity = vp',  isJumping = isJumping', obstacles = obstacles', gameStatus = gameStatus'}
  
  where
    (x, y) = playerPos gs
    vy     = playerVelocityY gs
    vy'    = vy - 3000 * dt  -- Gravity
    y'     = max (-15) (y + vy * dt)

    isJumping' = if y' <= 5 then False else isJumping gs

    p      = if isJumping' then playerRotation gs else 0
    vp     = playerRotationVelocity gs
    vp'    = if isJumping' then vp * 0.95 else 0
    p'     = p + vp * dt

    obstacles' = fmap (\(z, w) -> ((z - (400 * dt)), w)) (obstacles gs)

    gameStatus' = case obstacles' of 
      ((ox, _):_)
        | any (colidesWithPlayer (x, y')) obstacles' -> GameOver  
        | ox < (-11000) -> GameWon 
        | otherwise -> gameStatus gs
      [] -> gameStatus gs

{-| The main method that first runs tests, and then runs the game -}
main :: IO ()
main = do
  putStrLn "Running QuickCheck Tests ... "
  runTests
  putStrLn "Starting game"
  play window background 60 (initialState MainMenu) drawGame handleEvent updateGame





----------------------
-- QuickCheck Tests --
----------------------

{-|  Testing the helperfunction colidesWithPlayer
     The collisioncalculations are important because they are central for determining when it is Game Over. 
     The player is 70 x 70 units big, and we are testing for collisions in the following cases 
-}
collisionTests :: [Position -> Bool]
collisionTests = [
  \a -> colidesWithPlayer a (first (+69) a) == True,
  \a -> colidesWithPlayer a (second (+69) a) == True, 
  \a -> colidesWithPlayer a (first (+71) a) == False,
  \a -> colidesWithPlayer a (second (+71) a) == False
  ]

{-| Function that runs all tests, used in the main function -}
runTests :: IO ()
runTests = do 
  mapM_ quickCheck collisionTests