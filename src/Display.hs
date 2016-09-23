module Display where

import Graphics.UI.GLUT
import Data.IORef

import Global
import Board
import Logic

{- First let us fix the rendering attributes of blocks.
   At the moment tere is only the OpenGL color. -}

type FixedAttr = RenderingAttr (Vector3 GLfloat GLfloat GLfloat)

{- If the block is not going to be rendered, nothing is drawned
   and the coordinates are changed to the next block by the local
   function rot. If an attribute is passed, then it is used to draw
   a block and again rot is used. -}
drawBlock :: FixedAttr -> DisplayCallback
drawBlock x = 
  case x of
    NotRendered ->
      rot
    Attr c ->
      color c >> block >> color bl >> blockFrame >> rot
    where bl = Color3 0 0 (0 :: GLfloat)
          rot = rotate rotAngle (Vector3 0 0 tU)

{- A row is drawn simply bu drawing blocks one by one
   and using a translation to prepare the coordinates for the
   next row.  Finally the board is drawn by drawing the rows
   one by one.-}
drawRow :: [ FixedAttr ] -> DisplayCallback
drawRow cs =
  mapM_ drawBlock cs >> translate (Vector3 0 0 (tU * r))

drawBoard :: Board -> DisplayCallback
drawBoard b = mapM_ drawRow (getRows b)

{- Base is the polygonal shape at the bottom. -}
drawBase :: DisplayCallback
drawBase =
  sequence_ $ replicate width baseUnit
  where baseUnit =
          color (Color3 1 1 (1 :: GLfloat)) >>
          renderPrimitive LineLoop (vertices bottom) >>
          rotate rotAngle (Vector3 0 0 tU)

{- This is the display called by the GLUT loop. -}
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

{- This basically doesn't do anything for the moment.-}
reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)






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
        


