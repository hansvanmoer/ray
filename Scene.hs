module Scene (Scene, scene) where
import Camera
import Object

data Scene = S Camera [Object]

scene :: Camera -> [Object] -> Scene

scene camera objects = (S camera objects)
