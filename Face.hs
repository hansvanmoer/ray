module Face (Face) where
import Material
import Vector

-- A face
data Face = Face Material [Geometry]

-- A geometry primitive
data Geometry = Triangle Vector Vector Vector Vector -- A triangle consisting of three points and the normal
