import Prelude

forward :: (Floating a) => a -> a -> (a, a)
forward s l = (x, y)
    where x = l * cos s
          y = l * sin s