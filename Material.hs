module Material (Material) where
import Color

-- A material with a specified color and reflectivity
data Material = Colored Color Float

-- Creates a new colored material
coloredMaterial :: Color -> Float -> Material

coloredMaterial color reflectivity = Colored color (normalize reflectivity)
