module Camera (Camera, camera, Orientation, orientation, ViewPort, viewPort) where
import qualified Matrix as M
import Transform
import qualified Vector as V

-- The camera orientation as a set of euler angles
data Orientation = O Float Float Float

-- Creates an orientation based on euler angles
orientation :: Float -> Float -> Float -> Orientation

orientation alpha beta gamma = (O alpha beta gamma)

-- A viewport
data ViewPort = P Float Float Int Float

-- A viewport
viewPort :: Float -> Float -> Int -> Float -> ViewPort

viewPort focalDistance viewAngle pixels aspectRatio
  | focalDistance < 0 = error("focal distance must be > 0")
  | pixels > 0 = error("number of pixels must be > 0")
  | otherwise = (P focalDistance viewAngle pixels aspectRatio)

-- A camera
data Camera = C V.Vector Orientation ViewPort

-- Creates a camera
camera :: V.Vector -> Orientation -> ViewPort -> Camera

camera position orientation viewPort = (C position orientation viewPort)

-- The rays coming from a camera
rays :: Camera -> [(V.Vector, V.Vector)]

rays camera = map (ray camera) (M.positions pixels (round ((fromIntegral pixels) * aspectRatio)))
  where (C _ _ (P _ _ pixels aspectRatio)) = camera

-- A ray
ray :: Camera -> (Int, Int) -> (V.Vector, V.Vector)

ray camera (x, y) = (pos, (V.subtractVectors pos (focus camera)))
  where pos = V.addVectors (viewPortOrigin camera) (V.combineVectors (viewPortHorizontalUnit camera) (fromIntegral x) (viewPortVerticalUnit camera) (fromIntegral y))
  
-- The camera's focus point
focus :: Camera -> V.Vector

focus camera = rotateCameraVector camera (V.subtractVectors position [0, (- focalDistance), 0.0])
  where (C position _ (P focalDistance _ _ _)) = camera

-- The view port origin vector
viewPortOrigin :: Camera -> V.Vector

viewPortOrigin camera = rotateCameraVector camera [unit, 0.0, unit * aspectRatio]
  where (C _ _ (P focalDistance viewAngle _ aspectRatio)) = camera
        unit = - (tan (viewAngle / 2.0)) * focalDistance
        
-- The view port horizontal unit vector
viewPortHorizontalUnit :: Camera -> V.Vector

viewPortHorizontalUnit camera = rotateCameraVector camera [unit, 0.0, 0.0]
  where (C _ _ (P focalDistance viewAngle pixels _)) = camera
        unit = (tan viewAngle) * focalDistance / (fromIntegral pixels)
  
-- The view port vertical unit vector
viewPortVerticalUnit :: Camera -> V.Vector

viewPortVerticalUnit camera = rotateCameraVector camera [0.0, 0.0, unit * aspectRatio]
  where (C _ _ (P focalDistance viewAngle pixels aspectRatio)) = camera
        unit = (tan viewAngle) * focalDistance / (fromIntegral pixels)
  
-- Rotates a camera vector
rotateCameraVector :: Camera -> V.Vector -> V.Vector

rotateCameraVector (C _ (O alpha beta gamma) _) vector = transformVector (rotate alpha beta gamma) vector


