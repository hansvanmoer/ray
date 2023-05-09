module Image (Pixel, Raster) where

-- An RGBA tuple
type Pixel = (Float, Float, Float, Float)

-- A raster of pixels
type Raster = ([Pixel], Int, Int)
