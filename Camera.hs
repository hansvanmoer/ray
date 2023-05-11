module Camera (Camera, camera, ViewPoint, viewPoint) where
import qualified Matrix as M
import Transform
import qualified Vector as V

-- Camera view point
data ViewPoint = VP V.Vector Float Float Float

-- A camera viewpoint with a position and three euler angles defining the orientation
viewPoint :: V.Vector -> Float -> Float -> Float -> ViewPoint

viewPoint position alpha beta gamma = (VP position alpha beta gamma)


-- Camera properties
data Camera = C Float Float Int Float

-- Creates a camera with a given focal distance, view angle, number of horizontal pixels and aspectRatio
camera :: Float -> Float -> Int -> Float -> Camera

camera focalDistance viewAngle pixels aspectRatio = (C focalDistance viewAngle pixels aspectRatio)

-- The focus point of the camera
focus :: Camera -> V.Vector

focus (C focalDistance _ _ _) = [0.0, 0.0, - focalDistance]

-- Gets the camera aspect ratio
getAspectRatio :: Camera -> Float

getAspectRatio (C _ _ _ aspectRatio) = aspectRatio

-- The number of horizontal pixels in the view port
viewPortHorizontalPixels :: Camera -> Int

viewPortHorizontalPixels (C _ _ pixels _) = pixels

-- The number of vertical pixels in the view port
viewPortVerticalPixels :: Camera -> Int

viewPortVerticalPixels (C _ _ pixels aspectRatio) = round ((fromIntegral pixels) * aspectRatio)

-- The view port width
viewPortWidth :: Camera -> Float

viewPortWidth (C focalDistance viewAngle _ _) = (tan (viewAngle / 2.0)) * focalDistance

-- The view port height
viewPortHeight :: Camera -> Float

viewPortHeight camera = (viewPortWidth camera) * (getAspectRatio camera)

-- The view port origin vector of the camera
viewPortOrigin :: Camera -> V.Vector

viewPortOrigin camera = [- width / 2.0, height / 2.0, 0.0]
  where width = viewPortWidth camera
        height = viewPortHeight camera

-- The view port's horizontal unit vector
viewPortHorizontalUnit :: Camera -> V.Vector

viewPortHorizontalUnit camera = [width / pixels, 0.0, 0.0]
  where width = viewPortWidth camera
        pixels = fromIntegral (viewPortHorizontalPixels camera)

-- The view port's vertical unit vector
viewPortVerticalUnit :: Camera -> V.Vector

viewPortVerticalUnit camera = [0.0, height / pixels, 0.0]
  where height = viewPortHeight camera
        pixels = fromIntegral (viewPortVerticalPixels camera)
