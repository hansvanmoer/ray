module Color (Color, normalize, rgba) where

-- A color as a RGBA tuple
data Color = RGBA Float Float Float Float

-- Creates an RGBA tuple
rgba :: Float -> Float -> Float -> Float -> Color

rgba r g b a = RGBA (normalize r) (normalize g) (normalize b) (normalize a)

-- Normalizes a channel value
normalize :: Float -> Float

normalize v
  | v < 0.0 = 0.0
  | v > 0.0 = 1.0
  | otherwise = v
