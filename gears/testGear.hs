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

import Gear
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT
import Data.IORef
import Time

-- gears setup
-- gear teeth count is 11,20,9 to show, that Gear.hs toolbox is well parametrized.
-- just try to change placement of the gears, add some more or change number and shape of teeth

td = ToothDim 0.05 0.10 0.1
g1 = newMinimumRadiusGear td 0.6 0.1 0.05 blue
pg1 = PlacedGear g1 (Vector3 (-0.4) 0.4 0) 0 ClockWise Nothing
g2 = newMinimumRadiusGear td 0.3 0.1 0.1 red
pg2 = connectGear pg1 (-pi/2) g2
g3 = newMinimumRadiusGear td 0.3 0.1 0.2 green
pg3 = connectGear pg2 0 g3
g4 = newMinimumRadiusGear td 0.2 0.1 0.2 green
pg4 = connectGear pg3 (pi/3) g4

{-
-- gear setup using teeth count

g1 = newGear td 11 0.1 0.05 blue
pg1 = PlacedGear g1 (Vector3 (-0.4) 0.4 0) 0 ClockWise Nothing
g2 = newGear td 20 0.1 0.1 red
pg2 = connectGear pg1 (-pi/2) g2
td2 = ToothDim 0.05 0.1 0.1
g3 = newGear td2 9 0.1 0.2 green
pg3 = connectGear pg2 0 g3
g4 = newGear td2 9 0.1 0.2 green
pg4 = connectGear pg3 (pi/3) g4
-}

red    = Color4 0.8 0.1 0.0 (1.0::GLfloat)
green  = Color4 0.0 0.8 0.2 (1.0::GLfloat)
blue   = Color4 0.2 0.2 1.0 (1.0::GLfloat)


data GearState = GearState { lastTime :: ClockTime,
                             gsDirection :: Integer,  -- +1 or -1
                             phase :: GLfloat,
                             frameCount :: Integer } 

main = do
  --gears <- return [pg1, pg2, pg3]
  --gears <- return []
  --list <- makeCompiledList $ gear pg1
  time <- getClockTime
  gst <- newIORef $ GearState time 1 0 0
  (progName,_) <-  getArgsAndInitialize
  initialDisplayMode $= [RGBMode, WithDepthBuffer,DoubleBuffered]
  createWindow progName
  normalize          $= Enabled
  -- lighting
  lighting           $= Enabled
  position (Light 0) $= Vertex4 5.0 5.0 10.0 0.0
  light (Light 0)    $= Enabled 

  depthFunc $= Just Less
  -- callbacks
  gears <- mapM precomputeList [pg1, pg2, pg3, pg4]
  displayCallback $= display gst gears 
  keyboardMouseCallback $= Just (keyboard gst)
  idleCallback $= Just (timeStep gst)
  addTimerCallback (floor _FPS_MEASURE_INTERVAL * 1000) (measureFPS gst)
  reshapeCallback       $= Just reshape
  mainLoop

-- reshape and display rotation were copied from Shawn P. Garbett
-- I wanted to be near the glxgears without much work
reshape s@(Size w h) =
  do
    let r = (fromIntegral h)/(fromIntegral w)
    viewport     $= (Position 0 0, s)
    matrixMode   $= Projection
    loadIdentity
    frustum      (-1.0) 1.0 (-r) r 5.0 60.0
    matrixMode   $= Modelview 0
    loadIdentity
    translate    (Vector3 0 0 (-40.0::GLfloat))


display gst gears = do
  gState <- get gst
  clear [ColorBuffer,DepthBuffer]
  preservingMatrix $ do
    scale 7 7 (7::GLfloat)
    rotate 20 $ Vector3 (1.0::GLfloat) 0              0
    rotate 30 $ Vector3 0              (1.0::GLfloat) 0
    mapM_ (\pg -> drawPlacedGear pg (phase gState)) gears
    -- renderPrimitive Lines $ makeVertexes [pos pg1, pos pg2]
    -- drawPlacedGear pg3 curPhase
  swapBuffers


-- wheels are rotating with constant speed, only FPS varies


-- phase is a unit of rotation measured in angles of 1 tooth
-- phase == 1 means different angles for different gears
_PHASE_PER_SECOND = 5 :: GLfloat

diffSec ct1 ct2 = 
    fromTimeDiff $ diffClockTimes ct1 ct2

fromTimeDiff td = 
    (fromInteger . toInteger) (sum [secD, secH, secM, (tdSec td)]) + ((fromInteger . toInteger) (tdPicosec td) * 1e-12)
  where
    secM = 60 * (tdMin td)
    secH = 60 * 60 * (tdHour td)
    secD = 24 * 60 * 60 * (tdDay td)

timeStep gst = do
    gState <- get gst
    time <- getClockTime
    diffPhase <- return $ _PHASE_PER_SECOND * (fromInteger $ toInteger $ gsDirection gState) * (diffSec (lastTime gState) time)
    gst $= gState { phase = (phase gState) + diffPhase, lastTime = time, frameCount = frameCount gState + 1 }
    postRedisplay Nothing

-- FPS measuring is done by counting timeSteps and stoping every 5 seconds to calculate FPS
measureFPS gst = do
    gState <- get gst
    print $ "FPS: " ++ (show $ (fromInteger $ frameCount gState)/_FPS_MEASURE_INTERVAL)
    gst $= gState { frameCount = 0 }
    addTimerCallback (floor _FPS_MEASURE_INTERVAL*1000) (measureFPS gst)
    
_FPS_MEASURE_INTERVAL = 5 :: GLfloat
 
keyboard phase c _ _ _ = keyboardRot phase c

-- change gear rotation
keyboardRot gst (Char 'r') = do
  gState <- get gst
  gst $= gState { gsDirection = (gsDirection gState) * (-1) }

keyboardRot gst _ = return ()

