module Pieces where

import Data.Colour
import Data.Colour.Names
import Global

{- A piece has two components. The first component
   is the refernce point. The second component is an
   abstract piece which has a color and an infinite
   repeating list containing all possible orientaions
   of the piece. Each orientation is represented as
   a list of relative coordinates of its blocks. -}
type AbstractPiece = (Colour Float, [[Vector]])
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
shiftBy v (p, (c, vss)) = (p `plus` v, (c, vss))

-- Moving left, right and down, repectively.
moveL, moveR, moveD :: Piece -> Piece
moveL = shiftBy (-1, 0)
moveR = shiftBy (1, 0)
moveD = shiftBy (0,-1)

{- The rest are the actual abstract pieces and a list containing
   all the abstract pieces. -}

allPieces :: [AbstractPiece]
allPieces = [stickPiece]

stickPiece :: AbstractPiece
stickPiece = (blue, cycle vss) where
  vss = [[(3,0), (4,0), (5,0), (6,0)],
         [(6,-1), (6,0), (6,1), (6,2)]]

squarePiece :: AbstractPiece
squarePiece = (red, cycle vss) where
  vss = [[(5,-1), (6,-1), (5,0), (6,0)]]



