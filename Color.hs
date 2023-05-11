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

light :: Color -> Color -> Color

light (RGBA lr lg lb la) (RGBA br bg bb ba) = RGBA r g b ba 
  where
    r = lr * la + br * (1.0 - la)
    g = lg * la + bg * (1.0 - la)
    b = lb * la + bb * (1.0 - la)
