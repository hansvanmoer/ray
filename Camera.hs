module Camera (Camera, camera, ViewPoint, viewPoint) where
import qualified Matrix as M
import Ray
import Transform
import qualified Vector as V

-- A camera with specified properties located a view point
data Camera = C Properties ViewPoint

-- Constructs a camera
camera :: Properties -> ViewPoint -> Camera

camera props viewPoint = (C props viewPoint)

-- Gets the properties from the camera
getProperties :: Camera -> Properties

getProperties (C props _) = props

-- A ray from the camera through the pixel on the screen specified by the coordinates
cameraRay :: Camera -> (Int, Int) -> Ray

cameraRay (C props viewPoint) (x, y) = (ray pixel (V.subtractVectors pixel focus))
  where
    rotation = viewPointRotation viewPoint
    translation = viewPointTranslation viewPoint
    affine = concatenateTransforms rotation translation
    origin = transformVector affine (viewPortOrigin props)
    horizontalUnit = transformVector rotation (viewPortHorizontalUnit props)
    verticalUnit = transformVector rotation (viewPortVerticalUnit props)
    pixel = (V.addVectors origin (V.combineVectors horizontalUnit (fromIntegral x) verticalUnit (fromIntegral y)))
    focus = transformVector affine (viewPortFocus props)

-- All the rays emanating from the camera 
cameraRays :: Camera -> [Ray]

cameraRays camera = map (cameraRay camera) (M.positions width height)
  where
    props = getProperties camera
    width = viewPortHorizontalPixels props
    height = viewPortVerticalPixels props

-- Camera view point
data ViewPoint = VP V.Vector Float Float Float

-- A camera viewpoint with a position and three euler angles defining the orientation
viewPoint :: V.Vector -> Float -> Float -> Float -> ViewPoint

viewPoint position alpha beta gamma = (VP position alpha beta gamma)

-- The view point rotation transform
viewPointRotation :: ViewPoint -> Transform

viewPointRotation (VP _ alpha beta gamma) = (rotate alpha beta gamma)

-- The view point translation transform
viewPointTranslation :: ViewPoint -> Transform

viewPointTranslation (VP [x, y, z] _ _ _) = (translate x y z)

-- The camera properties
data Properties = P Float Float Int Float

-- Defines a camera with a given focal distance, view angle, number of horizontal pixels and aspectRatio
properties :: Float -> Float -> Int -> Float -> Properties

properties focalDistance viewAngle pixels aspectRatio = (P focalDistance viewAngle pixels aspectRatio)

-- The focus point of the viewport
viewPortFocus :: Properties -> V.Vector

viewPortFocus (P focalDistance _ _ _) = [0.0, 0.0, - focalDistance]

-- Gets the view port aspect ratio
getAspectRatio :: Properties -> Float

getAspectRatio (P _ _ _ aspectRatio) = aspectRatio

-- The number of horizontal pixels in the view port
viewPortHorizontalPixels :: Properties -> Int

viewPortHorizontalPixels (P _ _ pixels _) = pixels

-- The number of vertical pixels in the view port
viewPortVerticalPixels :: Properties -> Int

viewPortVerticalPixels (P _ _ pixels aspectRatio) = round ((fromIntegral pixels) * aspectRatio)

-- The view port width
viewPortWidth :: Properties -> Float

viewPortWidth (P focalDistance viewAngle _ _) = (tan (viewAngle / 2.0)) * focalDistance

-- The view port height
viewPortHeight :: Properties -> Float

viewPortHeight vp = (viewPortWidth vp) * (getAspectRatio vp)

-- The view port origin vector of the camera
viewPortOrigin :: Properties -> V.Vector

viewPortOrigin vp = [- width / 2.0, height / 2.0, 0.0]
  where width = viewPortWidth vp
        height = viewPortHeight vp

-- The view port's horizontal unit vector
viewPortHorizontalUnit :: Properties -> V.Vector

viewPortHorizontalUnit vp = [width / pixels, 0.0, 0.0]
  where width = viewPortWidth vp
        pixels = fromIntegral (viewPortHorizontalPixels vp)

-- The view port's vertical unit vector
viewPortVerticalUnit :: Properties -> V.Vector

viewPortVerticalUnit vp = [0.0, height / pixels, 0.0]
  where height = viewPortHeight vp
        pixels = fromIntegral (viewPortVerticalPixels vp)
