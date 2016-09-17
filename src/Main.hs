module Main where

import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock

import Global
import Logic
import Display
import Callbacks

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [ DoubleBuffered, WithDepthBuffer ]
  initialWindowSize $= Size 450 650
  _ <- createWindow "Cyletris"
  depthFunc $= Just Less 
  reshapeCallback $= Just reshape
  s <- initializeState
  sR <- newIORef s
  t0 <- getCurrentTime
  timer <- newIORef t0
  idleCallback $= Just (idle timer sR)
  displayCallback $= display sR
  keyboardMouseCallback $= Just (keyboardMouse sR)
  mainLoop
