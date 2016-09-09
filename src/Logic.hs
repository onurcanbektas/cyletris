module Logic where

import Global
import Pieces
import Board

{- There are three modes of the game: Playing,
   paused and finished. -} 
data Mode = Paused | Finished | Playing

{- These are the possible events. Designed as a separate
   abstraction to make the code more portable. Move request
   is a rotation or a shift request by the player most likely
   activated by a key stroke. Pause request pauses the
   game or restarts it if the game is paused or finished.
   Tick makes the game progress. -}
data Event = MoveReq (Piece -> Piece) | PauseReq | Tick

{- Game is determined by the mode, the board, the moving
   or current piece and the next piece. -}
type GameState = (Mode, Board, Piece, AbstractPiece)

{- This is the heart of the game. The piece input is the
   next piece. One could also omit that input and produce a
   piece randomly but this would put the output in the IO
   monad and testing would be more difficult. -} 
act :: Event -> AbstractPiece -> GameState -> GameState
act e p st = case st of
  -- If the game is paused one can only unpause it.
  (Paused, b, cp, np) ->
    case e of
      PauseReq ->
        (Playing, b, cp, np)
      _ ->
        st

  -- If the game is finished then one can only restart it.
  (Finished, b, cp, np) ->
    case e of
      PauseReq ->
        (Playing, emptyBoard, (iRef, np), p)
      _ ->
        st

  (Playing, b, cp, np) ->
    case e of
      PauseReq ->
        (Paused, b, cp, np)
      MoveReq f ->
        let cp' = f cp in
          if cp' `fits` b
             then (Playing, b, f cp', np)
             else st
      Tick ->
        let cp' = moveD cp in
          if cp' `fits` b
             then (Playing, b, cp', np)
             else (Playing, place cp' b, (iRef, np), p) 
                               
      
