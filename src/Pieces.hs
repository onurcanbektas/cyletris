module Pieces where

import Global

{- An abstract piece cocnsists of a drawing attribute and an infinite
   repeating list of possible orienttaions of a piece. Each orientation
   is encoded by a list of relative coordinates on the board. A board
   piece is an abstract piece together with a reference point. -}
data AbstractP a = AbstractP (DrawingAttr a) [[Vector]]
data BoardP a = BoardP Vector (AbstractP a)

{- Extracts the absolute coordinates of the blocks of a piece
   by shifting the relative coordinates by the refernce point. -}
coords :: BoardP a -> [Vector]
coords (BoardP p (AbstractP _ vss)) =
  map (plus p) (head vss)

{- Rotations to the left and to the right. Assumes that the
   repeating lists are ordered in a such a way that the next
   piece is obtained by a left rottaion. Rotations make sense
   at the level of abstract pieces by the encoding we use but they
   are used only for board pieces. -}
rotL, rotR :: BoardP a -> BoardP a
rotL (BoardP v (AbstractP x vss)) = 
  BoardP v (AbstractP x (tail vss))
rotR = rotL . rotL . rotL

shiftBy :: Vector -> BoardP a -> BoardP a
shiftBy v (BoardP w ap) =
  BoardP (v `plus` w) ap

-- Moving left, right and down, repectively.
moveL, moveR, moveD :: BoardP a -> BoardP a
moveL = shiftBy (-1, 0)
moveR = shiftBy (1, 0)
moveD = shiftBy (0,-1)

{- The rest are the abstract piece constructors and a list containing
   all them. Since the rendering attributes are abstract, instead of
   the actual pieces, we use piece constructors. Given an attribute,
   a constructor gives an abstract piece with that attribute. So
   constructors are just polymorphic functions.-}

allPieceConstructors :: [DrawingAttr a -> AbstractP a]
allPieceConstructors =
  [pieceI, pieceO, pieceL, pieceJ, pieceS, pieceZ, pieceT]

{- This is an auxiliary funciton which generates a contructor
   out of list of list of relative coordinates. -}
makeConstr :: [[Vector]] -> DrawingAttr a -> AbstractP a
makeConstr vss x =
  AbstractP x (cycle vss)

  
pieceI, pieceO, pieceL, pieceJ, pieceS, pieceZ, pieceT :: DrawingAttr a -> AbstractP a

pieceI = 
  makeConstr [[(3, 0), (4, 0), (5, 0), (6, 0)],
              [(6, -1), (6, 0), (6, 1), (6, 2)]]

pieceO = 
  makeConstr [[(5,-1), (6,-1), (5,0), (6,0)]]

pieceL = 
  makeConstr [[(3, 0), (4, 0), (5, 0), (3, -1)],
              [(4, -1), (4, 0), (4, 1), (5, -1)],
              [(3, 0), (4, 0), (5, 0), (5, 1)],
              [(3, 1), (4, -1), (4, 0), (4, 1)]]

pieceJ = 
  makeConstr [[(3, 0), (4, 0), (5, 0), (5, -1)],
              [(4, 1), (4, 0), (4, -1), (5, 1)],
              [(3, 0), (4, 0), (5, 0), (3, 1)],
              [(3, -1), (4, 1), (4, 0), (4, -1)]]
        
pieceS = 
  makeConstr [[(5, 0), (6, 0), (4, -1), (5, -1)],
              [(4, 0), (5, 0), (4, 1), (5, -1)]]

pieceZ = 
  makeConstr [[(5, 0), (6, 0), (6, -1), (7, -1)],
              [(6, 0), (7, 0), (7, 1), (6, -1)]]

pieceT = 
  makeConstr [[(3, 0), (4, 0), (5, 0), (4, -1)],
              [(4, -1), (4, 0), (5, 0), (4, 1)],
              [(3, 0), (4, 0), (5, 0), (4, 1)],
              [(4, -1), (3, 0), (4, 0), (4, 1)]]

