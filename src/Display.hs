module Display where

import Graphics.UI.GLUT
import Data.IORef

import Global
import Board
import Logic

display :: IORef GameState -> DisplayCallback
display sR = do
  clear [ DepthBuffer, ColorBuffer ]
  (m, b, p, _) <- get sR
  loadIdentity
  rotate 250 $ Vector3 1 0 (0 :: GLfloat)
  translate $ Vector3 0 0 ((-0.7) :: GLfloat)
  drawBase
  if m == Paused
     then drawBoard emptyBoard
     else drawBoard $ place p b
  swapBuffers

drawBase :: DisplayCallback
drawBase =
  sequence_ $ replicate width baseUnit
  where baseUnit =
          color (Color3 1 1 (1 :: GLfloat)) >>
          renderPrimitive LineLoop (vertices bottom) >>
          rotate rotAngle (Vector3 0 0 tU)

drawBlock :: GenericRGB -> DisplayCallback
drawBlock c@(r, g, b) =
  do if c == invisibleBlock
        then return ()
        else color (Color3 r g b) >>
             block >>
             color (Color3 0 0 (0 :: GLfloat)) >>
             blockFrame
     rotate rotAngle (Vector3 0 0 tU)

drawRow :: [ GenericRGB ] -> DisplayCallback
drawRow cs =
  mapM_ drawBlock cs >> translate (Vector3 0 0 (tU * r))

drawBoard :: Board -> DisplayCallback
drawBoard b = mapM_ drawRow (getRows b)

vertices :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
vertices vs =
  mapM_ vertex [Vertex3 x y z | (x, y, z) <- vs]

rotAngle :: GLfloat
rotAngle = 360 / fromIntegral width

theta, c, s, tU, r :: GLfloat
theta =  2 * pi / fromIntegral width
c = cos(theta)
s = sin(theta)
tU = 0.13
r = 0.35

top, bottom :: [(GLfloat, GLfloat, GLfloat)]
bottom = [(tU, 0, 0), (2*tU, 0, 0), (2*tU*c, 2*tU*s, 0),  (tU*c, tU*s, 0)]
top = map (\ (x, y, z) -> (x, y, z + tU * r)) bottom

blockFrame :: DisplayCallback
blockFrame =
  renderPrimitive LineLoop (vertices bottom) >>
  renderPrimitive LineLoop (vertices top)  >>
  renderPrimitive Lines (vertices between)
  where between = concat $ zipWith (\x y -> [x,y]) top bottom

block :: DisplayCallback
block =
  renderPrimitive Polygon (vertices top) >>
  renderPrimitive Polygon (vertices bottom) >>
  renderPrimitive Polygon (vertices left) >>
  renderPrimitive Polygon (vertices right) >>
  renderPrimitive Polygon (vertices front) >>
  renderPrimitive Polygon (vertices back)
  where left = [top !! 0, top !! 3, bottom !! 3, bottom !! 0]
        right = [top !! 1, top !! 2, bottom !! 2, bottom !! 1]
        front = [top !! 0, top !! 1, bottom !! 1, bottom !! 0]
        back = [top !! 3, top !! 2, bottom !! 2, bottom !! 3]
        
reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)

