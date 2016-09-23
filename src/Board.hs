module Board where

import Data.Array

import Global
import Pieces

{- A board is simply a two dimensional array of drawing attributes.
   Only static pieces will be held on the board. The moving piece
   is handeled separately using BoardP. -}
data Board a = Board (Array (Int, Int) (DrawingAttr a))

{- All entries of the emptyboard are NotDrawn. -}
emptyBoard :: Board a
emptyBoard =
  Board (array ((0, 0), (width - 1, height - 1)) notDrawns)
  where notDrawns = [((i, j), NotDrawn) | i <- [0..width - 1],
                                          j <- [0..height - 1]]

{- A piece fits a board if no y-coordinate of the the piece
   is negative and the board is empty where the blocks of the
   piece are supposed to be placed. This uses lazyness of && to
   avoid trying to access cells with negative index. -}
fits :: BoardP a -> Board a -> Bool
fits p (Board b) = 
  noNegatives && noClashes
  where noNegatives = and [snd v >= 0 | v <- vs]
        noClashes   = all (not . drawn) [b ! v | v <- vs]
        vs          = coords p

{- Places a board piece on the board. -}
place :: BoardP a -> Board a -> Board a
place p@(BoardP _ (AbstractP x _)) (Board b) =
  Board (b // [(v, x) | v <- coords p])

{- Utility function to copy rows from an array to a list of lists. -}
getRows :: Board a -> [[ DrawingAttr a ]]
getRows (Board b) = [getRow i b | i <- [0..height - 1]]
  where getRow i b = [ b ! (j, i) | j <- [0..width - 1]]
  
{- Clears the completed lines. More readable than optimized. Performance
   is not really an issue for a 10 by 22 board. -}
clearBoard :: Board a -> Board a
clearBoard =
  fromRows . extend . filter (any drawn) . getRows
  where extend xss =
          xss ++ replicate (height - length xss) notDrawnRow
        notDrawnRow =
          replicate width NotDrawn
        fromRows rs =
          Board (array ((0, 0), (width - 1, height - 1))
                 [((i, j), c) | (r, j) <- zip rs [0..],
                                (c, i) <- zip r [0..]])
                                                      


