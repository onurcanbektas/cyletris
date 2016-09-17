module Board where

import Data.Array

import Global
import Pieces

{- A board is simply a two dimensional array of colors.
   Only static pieces will be held on the board. The
   moving piece is handeled separately. -}
type Board = Array (Int, Int) GenericRGB

-- Empty cells are represented by black.
emptyBoard :: Board
emptyBoard =
  array ((0, 0), (width - 1, height - 1)) invisibleBlocks 
  where invisibleBlocks = [((i, j), invisibleBlock) | i <- [0..width - 1],
                                                      j <- [0..height - 1]]

{- A piece fits a board if no y-coordinate of the the piece
   is negative and the board is empty where the blocks of the
   piece are supposed to be placed. This uses lazyness of && to
   avoid trying to access cells with negative index. -}
fits :: Piece -> Board -> Bool
fits p b = 
  noNegatives && noClashes
  where noNegatives = and [snd v >= 0 | v <- vs]
        noClashes   = and [(b ! v) == invisibleBlock | v <- vs]
        vs          = coords p

-- Places the piece on the board.
place :: Piece -> Board -> Board
place p@(_, (c, _)) b =
  b // [(v, c) | v <- coords p]

-- Utility function to copy a row from an array to a list.
getRows :: Board -> [[ GenericRGB ]]
getRows b = [getRow i b | i <- [0..height - 1]]
  where getRow i b = [ b ! (j, i) | j <- [0..width - 1]]
  
-- Clears the completed lines.
clearBoard :: Board -> Board
clearBoard =
  fromRows . extend . filter nonEmpty . getRows
  where nonEmpty =
          any (== invisibleBlock)
        extend xss =
          xss ++ replicate (height - length xss) invisibleRow
        invisibleRow =
          replicate width invisibleBlock
        fromRows rs =
          array ((0, 0), (width - 1, height - 1))
                [((i, j), c) | (r, j) <- zip rs [0..],
                               (c, i) <- zip r [0..]]
                                                      
