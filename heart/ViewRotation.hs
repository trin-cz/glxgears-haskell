module ViewRotation where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT

_ROTATION_STEP = 2 :: GLfloat

keyboard c _ _ _ = keyboardRot c

keyboardRot (Char 'k') = do
  matrixMode $= Modelview 0
  rotate _ROTATION_STEP $ (Vector3 1 0 (0::GLfloat))
  postRedisplay Nothing

keyboardRot (Char 'j') = do
  matrixMode $= Modelview 0
  rotate (-_ROTATION_STEP) $ (Vector3 1 0 (0::GLfloat))
  postRedisplay Nothing

keyboardRot (Char 'l') = do
  matrixMode $= Modelview 0
  rotate _ROTATION_STEP $ (Vector3 0 1 (0::GLfloat))
  postRedisplay Nothing

keyboardRot (Char 'h') = do
  matrixMode $= Modelview 0
  rotate (-_ROTATION_STEP) $ (Vector3 0 1 (0::GLfloat))
  postRedisplay Nothing

keyboardRot _ = return ()
