{-# LANGUAGE ExistentialQuantification #-}
module Pieces where

import Global

{- A piece has two components. The first component
   is the refernce point. The second component is an
   abstract piece which has a color and an infinite
   repeating list containing all possible orientaions
   of the piece. Each orientation is represented as
   a list of relative coordinates of its blocks. -}
type AbstractPiece = (GenericRGB, [[Vector]])
type Piece = (Vector, AbstractPiece)

-- Extracts the absolute coordinates of the blocks of a piece.
coords :: Piece -> [Vector]
coords (p, (_, vss)) = map (plus p) (head vss)

{- Rotations to the left and to the right. Assumes that the
   repeating lists are ordered in a such a way that the next
   piece is obtained by a left rottaion. -}
rotL, rotR :: Piece -> Piece
rotL (p, (c, vss)) = (p, (c, tail vss))
rotR = rotL . rotL . rotL

shiftBy :: Vector -> Piece -> Piece
shiftBy v (p, ap) = (p `plus` v, ap)

-- Moving left, right and down, repectively.
moveL, moveR, moveD :: Piece -> Piece
moveL = shiftBy (-1, 0)
moveR = shiftBy (1, 0)
moveD = shiftBy (0,-1)

{- The rest are the actual abstract pieces and a list containing
   all the abstract pieces. -}

allPieces :: [AbstractPiece]
allPieces = [pieceI, pieceO, pieceL, pieceJ, pieceS, pieceZ, pieceT]

pieceI :: AbstractPiece
pieceI = ((0, 1, 0), cycle vss) where
  vss = [[(3, 0), (4, 0), (5, 0), (6, 0)],
         [(6, -1), (6, 0), (6, 1), (6, 2)]]

pieceO :: AbstractPiece
pieceO = ((1, 0, 0), cycle vss) where
  vss = [[(5,-1), (6,-1), (5,0), (6,0)]]

pieceL :: AbstractPiece
pieceL = ((0.5, 0.3, 0), cycle vss) where
  vss = [[(3, 0), (4, 0), (5, 0), (3, -1)],
         [(4, -1), (4, 0), (4, 1), (5, -1)],
         [(3, 0), (4, 0), (5, 0), (5, 1)],
         [(3, 1), (4, -1), (4, 0), (4, 1)]]

pieceJ :: AbstractPiece
pieceJ = ((0, 0.5, 0.7), cycle vss) where
  vss = [[(3, 0), (4, 0), (5, 0), (5, -1)],
         [(4, 1), (4, 0), (4, -1), (5, 1)],
         [(3, 0), (4, 0), (5, 0), (3, 1)],
         [(3, -1), (4, 1), (4, 0), (4, -1)]]
        
pieceS :: AbstractPiece
pieceS = ((0.5, 1, 0.4), cycle vss) where
  vss = [[(5, 0), (6, 0), (4, -1), (5, -1)],
         [(4, 0), (5, 0), (4, 1), (5, -1)]]

pieceZ :: AbstractPiece
pieceZ = ((0.1, 0.5, 0.9), cycle vss) where
  vss = [[(5, 0), (6, 0), (6, -1), (7, -1)],
         [(6, 0), (7, 0), (7, 1), (6, -1)]]

pieceT :: AbstractPiece
pieceT = ((0, 0, 1), cycle vss) where
  vss = [[(3, 0), (4, 0), (5, 0), (4, -1)],
         [(4, -1), (4, 0), (5, 0), (4, 1)],
         [(3, 0), (4, 0), (5, 0), (4, 1)],
         [(4, -1), (3, 0), (4, 0), (4, 1)]]

