module Field (Field, divide, identity, invert, multiply) where

import qualified AdditiveGroup as AG

class (AG.AdditiveGroup a) => Field a where
  -- Divides two values, if possible
  divide :: a -> a -> Maybe a

  -- The identity under multiplication
  identity :: a

  -- Inverts a value if possible
  invert :: a -> Maybe a
  
  -- Multiplies two values to produce a third
  multiply :: a -> a -> a

instance Field Float where
  divide l r = if r == 0.0
               then
                 Nothing
               else
                 Just (l / r)
  identity = 1.0

  invert v = if v == 0.0
             then
               Nothing
             else
               Just (1.0 / v)
  multiply = (*)
