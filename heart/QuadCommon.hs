module QuadCommon where

import Graphics.Rendering.OpenGL

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

drawSmoothQuads quads normalQuads = do
    mapM_ (uncurry drawVertexWithNormal) $ concat $ zipWith zip quads normalQuads
  where
    drawVertexWithNormal vert norm = do
        currentNormal $= norm
        vertex vert
 
