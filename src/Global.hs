module Global where

-- Dimesnions of the board.
width, height, visibleHeight :: Int
width = 10
height = 22
visibleHeight = 20


{- General vector type to work on a cylinder. -}
type Vector = (Int, Int)

{- Vector addition on a cylinder. This, together
   with (0,0), makes Vector an instance of the monoid
   class. Actually one can define a new instance 
   for equality too but that does not really add much
   to the quality of the code. So I will omit it for now. -}
plus :: Vector -> Vector -> Vector
plus (x1, y1) (x2, y2) =
  ((x1 + x2) `mod` width, y1 + y2)

-- Initial reference point for pieces.
iRef :: Vector
iRef = (0, visibleHeight - 1)



{- Rendering attributes are the attributes used
   to render a block. For the current version this
   just holds the color but could be more general.
   Since some blocks are not rendered this is
   actually a synonym for Maybe. Instead of assuming
   an Eq instance for a, I just defined an drawn function. -}
data DrawingAttr a = NotDrawn | Attr a

drawn :: DrawingAttr a -> Bool
drawn x = case x of
  NotDrawn -> False
  _ -> True



