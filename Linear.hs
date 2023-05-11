module Linear (solve) where
import Data.List

solve :: [[Float]] -> Maybe [Float]

solve [] = Just []

solve equations = case triangulate equations of
  Nothing -> Nothing
  Just triangle -> Just (substituteAll (reverse (triangle)) [])

-- Triangulates a system, turning it into an upper triangular system
triangulate :: [[Float]] -> Maybe [[Float]]

triangulate [] = Just []

triangulate input = case (pivot input) of
  Nothing -> Nothing
  Just [] -> Just []
  Just (x:xs) -> case triangulate (map (eliminate x) xs) of
    Nothing -> Nothing
    Just tail -> Just (x: tail)
  
-- reorders the equations to ensure a pivot exists at the head of the list
pivot :: [[Float]] -> Maybe [[Float]]

pivot [] = Just []

pivot equations = if (head x) == 0.0 then Nothing else Just (x:xs)
  where (x:xs) = sortBy comparePivots equations

-- Compares two equations to see which one is the best pivot
comparePivots :: [Float] -> [Float] -> Ordering

comparePivots [] [] = EQ

comparePivots left right = compare (abs (head right)) (abs (head left))

-- Eliminates the first column in a row
eliminate :: [Float] -> [Float] -> [Float]

eliminate [] [] = []

eliminate [pivot] [x] = []

eliminate [0] row = error("can not eliminate without non-zero pivot")

eliminate (pivot:ps) (factor:xs) = (combine pivot factor ps xs) 

-- Combines two rows
combine :: Float -> Float -> [Float] -> [Float] -> [Float]

combine _ _ [] [] = []

combine pivot coef (p:ps) (e:es) = ((e * pivot) - (p * coef)):(combine pivot coef ps es)

-- Substitutes all variables
substituteAll :: [[Float]] -> [Float] -> [Float]

substituteAll [] vars = vars

substituteAll (x:xs) vars = case (substitute x vars) of
  [] -> []
  nvars -> substituteAll xs nvars
 
-- Substitutes variables into an equation and places it at the end of the list of variables
substitute :: [Float] -> [Float] -> [Float]

substitute [] [] = []

substitute [] vars = error("empty equation but non empty variable list")

substitute (coef: coefs) vars
  | (length coefs) /= ((length vars) + 1) = error("wrong number of terms in equation or variable list")
  | coef == 0 = []
  | otherwise =(- sum / coef):vars
    where sum = (foldr (+) 0.0 (zipWith (*) coefs (vars ++ [1.0])))
