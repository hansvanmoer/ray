module Model (Model) where
import Face
import Material

data Model = Faces [Face] | Sphere Float Material
