module Heart where

import Graphics.Rendering.OpenGL
import Data.Array
import QuadCommon

heartFunction u t = Vertex3 x y z
    where x = (abs u) * (sin t) * (sin u) 
          y = (abs u) * (sin t) * (cos u)
          z = 0.3*(abs u) * (sin t) * (cos t)

heartNormal u t = fromVector $ vectorProduct (Vector3 dxdt dydt dzdt) (Vector3 dxdu dydu dzdu) 
--heartNormal u t = fromVector $ vectorProduct (Vector3 dxdu dydu dzdu) (Vector3 dxdt dydt dzdt) 
  where
    dxdu = (abs u)*(sin t)*(cos u) + (signum u) * (sin t) * (sin u)
    dydu = (abs u)*(sin t)*(-1)*(sin u) + (signum u) * (sin t) * (cos u)
    dzdu = 0.3 *(abs u)*(sin t)*(cos t)
    dxdt = (abs u)*(cos t)*(sin u)
    dydt = (abs u)*(cos t)*(cos u)
    dzdt = 0.3 *(abs u)*(cos t)*(cos t) + (abs u)*(sin t)*(-1)*(sin t) 

data Interval = Interval { min, max :: GLfloat }

samplerArray count (Interval a_min a_max) (Interval b_min b_max) =
    array ((0,0), (count,count)) $
          [ ( (a_step,b_step), f_step a_step b_step ) | a_step <- [0..count], b_step <- [0..count] ]
  where
    f_step a_step b_step = (a_min + a_unit * (fromIntegral a_step), b_min + b_unit * (fromIntegral b_step) )
    a_diff = a_max - a_min
    b_diff = b_max - b_min
    a_unit = a_diff / (fromIntegral count)
    b_unit = b_diff / (fromIntegral count)

heartVertexArray res = fmap (uncurry heartFunction) $ samplerArray res (Interval (-pi) (pi)) (Interval 0 (pi)) 
heartNormalArray res = fmap (uncurry heartNormal) $ samplerArray res (Interval (-pi) (pi)) (Interval 0 (pi)) 

quadsFromArray arr = [ formQuad i j | i <- [i_min..(i_max-1)], j <- [j_min..(j_max-1)] ]
  where  
    ((i_min,j_min),(i_max,j_max)) = bounds arr
    formQuad i j = [ arr ! idx | idx <- [ (i,j) , (i+1,j) , (i+1,j+1) , (i,j+1) ] ]
