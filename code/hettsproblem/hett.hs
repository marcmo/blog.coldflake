import Data.List
import Data.Function

lsort = sortBy (compare `on` length)

lfsort xs = let ys = groupBy (\x y->length x == length y) (lsort xs) in
  concat $ sortBy (compare `on` length) ys
