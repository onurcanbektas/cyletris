module Callbacks where

import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock

import Logic
import Pieces

-- Idle callback handles the ticking.
idle :: IORef UTCTime -> IORef GameState -> IdleCallback
idle timer sR =
  do (m, _, _, _) <- get sR
     if m == Playing 
        then do t0 <- get timer
                t1 <- getCurrentTime
                if toRational (diffUTCTime t1 t0) > 1.2
                   then act Tick sR >> timer $= t1
                   else return ()
        else return ()
     postRedisplay Nothing


--This binds the abstract act function to the user interface.
keyboardMouse :: IORef GameState -> KeyboardMouseCallback
keyboardMouse sR key Down _ _ =
  case key of
    SpecialKey KeyLeft ->
      act (MoveReq moveL) sR
    SpecialKey KeyRight ->
      act (MoveReq moveR) sR
    SpecialKey KeyUp ->
      act (MoveReq rotL) sR
    SpecialKey KeyDown ->
      act (MoveReq moveD) sR
    Char 'p' ->
      act PauseReq sR
    Char 'P' ->
      act PauseReq sR
    Char ' ' ->
      act Tick sR
    _ -> return ()
keyboardMouse _ _ _ _ _ = return ()
