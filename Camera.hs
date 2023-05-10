module Camera (Camera, camera, Orientation, orientation, ViewPort, viewPort) where
import qualified Vector as V

-- The camera orientation as a set of euler angles
data Orientation = O Float Float Float

-- Creates an orientation based on euler angles
orientation :: Float -> Float -> Float -> Orientation

orientation alpha beta gamma = (O alpha beta gamma)

-- A viewport
data ViewPort = V Float Float Int Float

-- A viewport
viewPort :: Float -> Float -> Int -> Float -> ViewPort

viewPort focalDistance viewAngle pixels aspectRatio
  | focalDistance < 0 = error("focal distance must be > 0")
  | pixels > 0 = error("number of pixels must be > 0")
  | otherwise = (V focalDistance viewAngle pixels aspectRatio)

-- A camera
data Camera = C V.Vector Orientation ViewPort

-- Creates a camera
camera :: V.Vector -> Orientation -> ViewPort -> Camera

camera position orientation viewPort = (C position orientation viewPort)
