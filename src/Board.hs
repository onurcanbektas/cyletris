module Board where

import Global
import Pieces
import Data.Colour
import Data.Colour.Names (black)
import Data.Array

{- A board is simply a two dimensional array of colors.
   Only static pieces will be held on the board. The
   moving piece is handeled separately. -}
type Board = Array (Int, Int) (Colour Float)

-- Empty cells are represented by black.
emptyBoard :: Board
emptyBoard =
  array ((0, 0), (width - 1, height - 1)) blacks
  where blacks = [((i, j), black) | i <- [0..width], j <- [0..height]]

{- A piece fits a board if no y-coordinate of the the piece
   is negative and the board is empty where the blocks of the
   piece are supposed to be placed. This uses lazyness of && to avoid
   trying to access cells with negative index. -}
fits :: Piece -> Board -> Bool
fits p b =
  noNegatives && noClashes
  where noNegatives = and [snd v >= 0 | v <- vs]
        noClashes   = and [(b ! v) /= black | v <- vs]
        vs          = coords p

-- Places the piece on the board.
place :: Piece -> Board -> Board
place p@(_, (c, _)) b =
  b // [(v, c) | v <- coords p]

-- Clears the completed lines.
clear :: Board -> Board
clear =
  fromRows . extend . filter nonEmpty . getRows
  where getRows b =
          [getRow i b | i <- [0..height - 1]]
        getRow i b =
          [ b ! (i, j) | j <- [0..width - 1]]
        nonEmpty =
          any (/= black)
        extend xss =
          xss ++ replicate (height - length xss) blackRow
        blackRow =
          replicate width black
        fromRows rs =
          array ((0, 0), (width - 1, height - 1))
                [((i, j), c) | (r, i) <- zip rs [0..], (c, j) <- zip r [0..]]
                                                      
