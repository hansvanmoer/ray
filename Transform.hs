module Transform (Transform) where
import qualified Matrix as M
import qualified Vector as V

-- A simple 3D affine transform
data Transform = T M.Matrix M.Matrix

-- The identity transform
identity :: Transform

identity = T (M.identity 3) (M.zero 3 1)

-- A translation
translate :: Float -> Float -> Float -> Transform

translate dx dy dz = T (M.identity 3) (M.column [dx, dy, dz])

-- A scale
scale :: Float -> Float -> Float -> Transform

scale rx ry rz = T (M.multiplyMatrices (M.column [rx, ry, rz]) (M.identity 3)) (M.zero 3 1)

-- A uniform scale
uniformScale :: Float -> Transform

uniformScale r = T (M.multiplyWithScalar (M.identity 3) r) (M.zero 3 1)
                 
-- A rotation around euler angles
rotate :: Float -> Float -> Float -> Transform

rotate rx ry rz = T (M.square 3 [
                        (cos ry) * (cos rz),
                        (sin rx) * (sin ry) * (cos rz) - (cos rx) * (sin rz),
                        (cos rx) * (sin ry) * (cos rz) + (sin rx) * (sin rz),
                        
                        (cos ry) * (sin rz),
                        (sin rx) * (sin ry) * (sin rz) + (cos rx) * (cos rz),
                        (cos rx) * (sin ry) * (sin rz) - (sin rx) * (cos rz),
                        
                        (sin ry),
                        (sin rx) * (cos ry),
                        (cos rx) * (cos ry)]) (M.zero 3 1)

-- Concatenates two transforms
concatenate :: Transform -> Transform -> Transform

concatenate (T lLinear lTrans) (T rLinear rTrans) = T (M.multiplyMatrices rLinear lLinear) (M.addMatrices (M.multiplyMatrices rLinear lTrans) rLinear)

-- Transforms a vector
transformVector :: Transform -> V.Vector -> V.Vector

transformVector (T linear trans) vector = (M.getColumn (M.addMatrices (M.multiplyMatrices linear vectorMatrix) trans) 0)
  where vectorMatrix = M.column(vector)
