module Material (Material) where
import Color

-- A material with a specified color and reflectivity
data Material = Colored Color Float
