module Display (idle, display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Cube
import Points
 
s :: GLfloat
s = 0.7

cubeSize = 0.15

display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display angle pos = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity
  (x',y') <- get pos
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    a <- get angle
    rotate a $ Vector3 0 0 1
    rotate a $ Vector3 0 0.9 1 -- changed y-component a bit to show off cube corners
    scale s s s
    forM_ (points 7) $ \(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 x y z
      cube cubeSize
      color $ Color3 (0::GLfloat) 0 0 -- set outline color to black
      cubeFrame cubeSize -- draw the outline
  swapBuffers
 
idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
  postRedisplay Nothing