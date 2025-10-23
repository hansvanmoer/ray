module Continuum where

import qualified Field as F

-- A sufficiently dense continuum
class (F.Field a) => Continuum a where
  -- The square root of a value, if it exists
  squareRoot :: a -> Maybe a

instance Continuum Float where
  squareRoot v = if v >= 0.0
                 then
                   Just (sqrt v)
                 else
                   Nothing
