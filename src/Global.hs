module Global where

import Graphics.UI.GLUT

-- Dimesnions of the board.
width, height, visibleHeight :: Int
width = 10
height = 22
visibleHeight = 20


-- General vector type to work on a cylinder.
type Vector = (Int, Int)

-- Vector addition on a cylinder
plus :: Vector -> Vector -> Vector
plus (x1, y1) (x2, y2) =
  ((x1 + x2) `mod` width, y1 + y2)

-- Initial reference point for pieces.
iRef :: Vector
iRef = (0, visibleHeight - 1)


{- General RGB color type. Since no blending is
   involved no need to use Data.Colour.  -}
type GenericRGB = (GLfloat, GLfloat, GLfloat) 

-- Invisible blocks are coded as black.
invisibleBlock :: GenericRGB
invisibleBlock = (0, 0, 0)



