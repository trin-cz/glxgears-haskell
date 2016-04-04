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

module Gear where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.List

-- radius depends on teeth amount and dimensions. This is error with which radius is calculated 
_MAX_ERROR = 0.001

data ToothDim = ToothDim { top :: GLfloat, -- width at the top of a tooth
                           bottom :: GLfloat, -- width at the base of a tooth
                           height :: GLfloat } -- height of the tooth

data Gear = Gear { bottomRadius,      -- from center to tooth bottom corner
                   topRadius,         -- from center to tooth top corner
                   bottomAngle,       -- angle that tooth bottom line takes
                   topAngle,          -- angle that  tooth top line takes
                   betweenAngle,      -- angle between top and bottom tooth line
                   innerRadius,       -- inner circle of gear
                   thickness :: GLfloat, -- z axis thickness
                   gColor :: Color4 GLfloat
                 }

data PlacedGear = PlacedGear {
        gear :: Gear,
        pos  :: Vector3 GLfloat,      -- gear position
        basePhase :: GLfloat,         -- starting phase of rotation
        direction :: Direction,
        drawList :: Maybe (DisplayList)
    }

-- direction in which a gear rotates.  used for automatic connection of gears
-- connected gear rotates in opposite direction
data Direction = ClockWise | CounterClockWise deriving (Eq,Show)
numDirection ClockWise        = -1
numDirection CounterClockWise = 1

oppositeDirection ClockWise = CounterClockWise
oppositeDirection CounterClockWise = ClockWise

-- angles of gear contour points
angles gear = 0 : takeWhile (<2*pi) [ sum $ take i as | i <- [1..] ] ++ [0]
  where
    as = cycle $ map (\f -> f gear) [ bottomAngle, betweenAngle, topAngle, betweenAngle ]

-- angle of 1 phase
toothAngle gear = sum $ map (\f -> f gear) [bottomAngle, betweenAngle, topAngle, betweenAngle]

calculateGearRadius toothDim teethCount = 
      calculateGearRadius' toothDim teethCount 1000 0.5

calculateGearRadius' toothDim targetTeeth tmpRadius factor = 
        case countTeeth top bottom newRadius of
            tmpTeeth
                | tmpTeeth > targetTeeth + _MAX_ERROR -> calculateGearRadius' toothDim targetTeeth newRadius factor
                | tmpTeeth < targetTeeth              -> calculateGearRadius' toothDim targetTeeth tmpRadius (factor/2)
                | otherwise                           -> newRadius
    where
        ToothDim top bottom height = toothDim
        newRadius = tmpRadius*(1-factor)

countTeeth top bottom radius = 
  pi / ( asin (top / (2*radius)) + asin(bottom / (2*radius) ) )

newTeethCountGear toothDim teethCount innerR thickness c = 
    Gear bottomR topR bottomA topA betweenA innerR thickness c 
  where
    bottomR = calculateGearRadius toothDim teethCount
    topR    = sqrt ( (bottomR*bottomR) - (bottom*bottom/4) ) + height
    ToothDim top bottom height = toothDim
    bottomA = 2 * asin (top / (2*bottomR))
    topA    = 2 * asin (top / (2*(bottomR+height)))
    totalA  = 2 * pi / teethCount
    betweenA= (totalA - topA - bottomA) / 2

newGear = newTeethCountGear

newMinimumRadiusGear toothDim minRadius innerR thickness c = 
    newGear toothDim (fromIntegral teethCount + 1) innerR thickness c
  where
    ToothDim top bottom height = toothDim
    toothA = 2 * ( asin (top / (2*minRadius)) +  asin (bottom / (2*minRadius)) )
    (teethCount, _) = properFraction ( 2 * pi / toothA )

-- merges 2 circles - alternates between big and small circle and creates teeth
gearContourPoints gear = 
    zipWith3 zipF [0,1..] inCircle outCircle
  where
    zipF i inP outP | i `mod` 4 == 0 = inP
                    | i `mod` 4 == 1 = inP
                    | otherwise      = outP
    inCircle  = angleCirclePoints as $ bottomRadius gear
    outCircle = angleCirclePoints as $ topRadius    gear
    as = angles gear

gearFacePoints gear = 
    concat $ zipWith (\x y -> [x,y]) contourPs innerPs
  where
    contourPs = gearContourPoints gear
    innerPs   = angleCirclePoints (angles gear) (innerRadius gear)

gearTeethPoints gear = 
    concat $ zipWith (\x y -> [x,y]) (setZ contourPs (-0.5)) (setZ contourPs 0.5)
  where
    contourPs = gearContourPoints gear

gearInsidePoints gear = 
    concat $ zipWith (\x y -> [x,y]) (setZ contourPs (-0.5)) (setZ contourPs 0.5)
  where
    contourPs = angleCirclePoints (angles gear) (innerRadius gear)

angleCirclePoints angles radius = 
    map (\a -> point a radius) angles
  where 
    point a r = Vertex3 (r*(cos a)) (r*(sin a)) 0

setZ ps newZ = 
    map (\(Vertex3 x y z) -> Vertex3 x y newZ) ps

-- general rendering

fromRad r = 360 * r / 2 / pi

uncurry3 f (x,y,z) = f x y z

drawGear g drawList = do
  preservingMatrix $ do
    scale 1 1 (thickness g)
    rotate (fromRad $ bottomAngle g * (-0.5)) $ Vector3 0 0 (1::GLfloat)
    cl <- case drawList of
      Just l -> return l
      Nothing -> makeCompiledList g
    callList cl

-- takes a quadStrip and for every quad computes 1 normal (for flat shading)
computeNormals :: [Vertex3 GLfloat] -> [Normal3 GLfloat]
computeNormals (v1:v2:v3:v4:vs) = 
    (fromVector $ makeVector v1 v3 `vectorProduct` makeVector v1 v2) : (computeNormals $ v3:v4:vs)
computeNormals (_:_:[]) = []

makeVector (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = Vector3 (x2-x1) (y2-y1) (z2-z1)
vectorProduct (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    Vector3 (y1*z2 - y2*z1) (z1*x2 - z2*x1) (x1*y2 - x2*y1)

fromVector (Vector3 x y z) = Normal3 x y z

quadsFromStrip (v1:v2:[]) = []
quadsFromStrip (v1:v2:v3:v4:vs) = [ [v1,v2,v4,v3] ] ++ (quadsFromStrip (v3:v4:vs))

quadNormal v1 v2 v3 v4 = (fromVector $ makeVector v1 v3 `vectorProduct` makeVector v1 v2)
normalsFromQuads = map (\[v1,v2,v3,v4] -> quadNormal v1 v2 v3 v4)

normalsToCenter (v1:v2:qs) = n : normalsToCenter qs
    where 
      n = fromVector $ makeVector v $ Vertex3 0 0 (0::GLfloat)
      [v] = setZ [v1] 0
normalsToCenter [] = []

drawSmoothQuadStrip (v1:v2:vs) (n1:ns) = do
    currentNormal $= n1
    vertex v1
    vertex v2
    drawSmoothQuadStrip vs ns
drawSmoothQuadStrip [] [] = return ()

drawFlatQuads quads = do
    mapM_ drawFlatQuad $ zip quads normals
  where
    normals = normalsFromQuads quads
    drawFlatQuad (quad, n) = do
      currentNormal $= n
      mapM_ vertex quad
        
-- i have only recently found about DisplayList. Maybe precompilation would be faster.
makeCompiledList g = 
    defineNewList Compile $ do
      materialAmbientAndDiffuse Front $= gColor g
      shadeModel $= Flat 
      renderPrimitive QuadStrip $ do
        currentNormal $= Normal3 0 0 (1::GLfloat)
        mapM_ vertex (setZ faceP (0.5))
      renderPrimitive QuadStrip $ do
        currentNormal $= Normal3 0 0 ((-1)::GLfloat)
        mapM_ vertex (setZ faceP (-0.5))
      renderPrimitive Quads $ do
        drawFlatQuads $ quadsFromStrip $ teethP
      shadeModel $= Smooth 
      renderPrimitive QuadStrip $ do
        drawSmoothQuadStrip insideP $ normalsToCenter insideP
  where
    faceP = gearFacePoints g
    insideP = reverse $ gearInsidePoints g
    teethP = gearTeethPoints g
    

precomputeList pg = do
    cl <- makeCompiledList $ gear pg
    return $ pg { drawList = Just cl }

-- connects gear2 to gear1. Does not solve the distance, only finds out needed phase
-- of gear2
getCorespondingPhase pg1 pg2 = 
    newPhase
  where
    (_,newPhase)     = properFraction $ (angle2/(toothAngle g2)) + phaseOfTouch + 0.5
    (_,phaseOfTouch) = properFraction $ (angle1/(toothAngle g1)) - (basePhase pg1)
    angle1 = (2*pi) + atan2 (y2-y1) (x2-x1)
    angle2 = (2*pi) + atan2 (y1-y2) (x1-x2)
    Vector3 x1 y1 _ = pos pg1
    Vector3 x2 y2 _ = pos pg2
    g1 = gear pg1
    g2 = gear pg2

rotateGearToCorespond pg1 pg2 = 
    pg2 { basePhase = getCorespondingPhase pg1 pg2, direction = oppositeDirection $ direction pg1 }

-- places gear2 to minimum distance and sets phase do that teeth fit
connectGear pg1 angle g2 =
    rotateGearToCorespond pg1 $ PlacedGear g2 (Vector3 x2 y2 0) 0 ClockWise Nothing
  where
    g1 = gear pg1
    distance = (topRadius g1) + (bottomRadius g2)
    x2 = cos angle * distance + x1
    y2 = sin angle * distance + y1
    Vector3 x1 y1 _ = pos pg1

drawPlacedGear pg phase = do
    preservingMatrix $ do
      translate $ pos pg
      rotate angle $ Vector3 0 0 (1::GLfloat)
      drawGear (gear pg) (drawList pg)
  where
    angle = fromRad $ (dirPhase + basePhase pg) * (toothAngle $ gear pg)
    dirPhase = phase * (numDirection $ direction pg)
