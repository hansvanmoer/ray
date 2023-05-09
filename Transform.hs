module Transform (Transform) where

-- A simple 3D affine transform
data Transform = Matrix
                 Float Float Float Float
                 Float Float Float Float
                 Float Float Float Float

--rotate :: Float -> Float -> Float -> Transform

--rotate rx ry rz = ((cos ry) * (cos rz),
--                   (sin rx) * (sin ry) * (cos rz) - (cos rx) * (sin rz),
--                  (cos rx) * (sin ry) * (cos rz) + (sin rx) * (sin rz),
--                  0.0,
--                    
--                   (cos ry) * (sin rz),
--                   (sin rx) * (sin ry) * (sin rz) + (cos rx) * (cos rz),
--                   (cos rx) * (sin ry) * (sin rz) - (sin rx) * (cos rz),
--                   0.0,
                   
 --                  -(sin ry),
--                   (sin rx) * (cos ry),
--                   (cos rx) * (cos ry),
--                   0.0)

