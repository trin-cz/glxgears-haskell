-----------------------------------------------------------------------------
-- 
-- Copyright (c) 2008 Martin 'Trin' Kudlvasr
-- All rights reserved.
--
-- Redistribution and use in sourse and binary forms are permitted
-- provided that the above copyright notice and this paragraph are
-- duplicated in all such forms and that any documentation,
-- advertising materials, and other materials related to such
-- distribution and use acknowledge that the software was developed
-- by Martin Kudlvasr. The name of Martin Kudlvasr 
-- may not be used to endorse or promote products derived from this
-- software without specific prior written permission.
-- THIS SOFTWARE IS PROVIDED 'AS IS' AND WITHOUT ANY EXPRESS OR
-- IMPLIED WARRANTIES, INCLUDING LIMITATION, THE IMPLIED
-- WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
--
-------------------------------------------------------------------------------

-----------------------------------------------------------------------------
--
-- Author -
--     Martin 'Trin' Kudlvasr
--     www.trinpad.eu
--     November, 2008

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT
import ViewRotation
import QuadCommon
import Heart

red = Color4 0.8 0.1 0.0 (1.0::GLfloat)
green  = Color4 0.0 0.8 0.2 (1.0::GLfloat)
blue   = Color4 0.2 0.2 1.0 (1.0::GLfloat)

_RESOLUTION = 50

heartQuads = quadsFromArray $ heartVertexArray _RESOLUTION
heartNormalQuads = quadsFromArray $ heartNormalArray _RESOLUTION

precomputeList2 = defineNewList Compile $ do
    materialAmbientAndDiffuse Front $= red
    renderPrimitive Quads $ drawSmoothQuads heartQuads heartNormalQuads

precomputeList  = defineNewList Compile $ do
    materialAmbientAndDiffuse Front $= red
    renderPrimitive Quads $ drawFlatQuads heartQuads



drawAxes = do 
    materialAmbientAndDiffuse Front $= red
    renderPrimitive Lines $ do
        vertex $ Vertex3 0 0 (0::GLfloat)
        vertex $ Vertex3 2 0 (0::GLfloat)
    materialAmbientAndDiffuse Front $= green
    renderPrimitive Lines $ do
        vertex $ Vertex3 0 0 (0::GLfloat)
        vertex $ Vertex3 0 2 (0::GLfloat)
    materialAmbientAndDiffuse Front $= blue
    renderPrimitive Lines $ do
        vertex $ Vertex3 0 0 (0::GLfloat)
        vertex $ Vertex3 0 0 (2::GLfloat)
        

main = do
  (progName,_) <-  getArgsAndInitialize
  initialDisplayMode $= [RGBMode, WithDepthBuffer,DoubleBuffered]
  createWindow progName
  normalize          $= Enabled
  -- lighting
  lighting           $= Enabled
  position (Light 0) $= Vertex4 5.0 5.0 10.0 0.0
  light (Light 0)    $= Enabled 
  list <- precomputeList2
  depthFunc $= Just Less
  -- callbacks
  displayCallback $= display list 
  keyboardMouseCallback $= Just keyboard
  mainLoop

_SCALE = 0.3 :: GLfloat

display list = do
  clear [ColorBuffer,DepthBuffer]
  preservingMatrix $ do
    scale _SCALE _SCALE _SCALE
    drawAxes 
    callList list
  swapBuffers


