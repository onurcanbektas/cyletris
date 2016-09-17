module Logic where

import Data.IORef
import System.Random

import Global
import Pieces
import Board

{- There are three modes of the game: Playing,
   paused and finished. -} 
data Mode = Paused | Finished | Playing
  deriving (Eq)

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

-- This is a utility function which genertaes a random piece.
pick :: IO AbstractPiece
pick =
  do i <- randomRIO (0, length allPieces - 1)
     return $ allPieces !! i

-- Initializes the game with the randomly picked current and next pieces.
initializeState :: IO GameState
initializeState =
  do p1 <- pick
     p2 <- pick
     return (Playing, emptyBoard, (iRef, p1), p2) 

{- This is the heart of the game. -} 
act :: Event -> IORef GameState -> IO ()
act e sR =
  do st <- readIORef sR
     case (st, e) of
       ((Paused, b, cp, np), PauseReq) ->
         sR `writeIORef` (Playing, b, cp, np)
       ((Paused, _, _, _), _) ->
         return ()

       ((Finished, b, cp, np), PauseReq) ->
         initializeState >>= (sR `writeIORef`)
       ((Finished, _, _, _), _) ->
         return ()

       ((Playing, b, cp, np), PauseReq) ->
         sR `writeIORef` (Paused, b, cp, np)
       ((Playing, b, cp, np), MoveReq f) ->
         let cp' = f cp in if cp' `fits` b
                              then sR `writeIORef` (Playing, b, cp', np)
                              else return ()
       ((Playing, b, cp, np), Tick) ->
         let cp' = moveD cp in
             if cp' `fits` b
                then sR `writeIORef` (Playing, b, cp', np)
                else do let b' = clearBoard $ place cp b
                            np' = (iRef, np)
                        p' <- pick
                        if np' `fits` b'
                           then sR `writeIORef` (Playing, b', (iRef, np), p')
                           else sR `writeIORef` (Finished, b', (iRef, np), p')
                               
      
