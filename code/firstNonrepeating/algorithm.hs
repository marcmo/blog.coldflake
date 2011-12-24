import Data.List(group,find,sort)

firstNonRepeating ::  Ord a => [a] -> Maybe a
firstNonRepeating xs = walkTill uniques xs where
  uniques = [head ys | ys <- group $ sort xs
                     , ((==) 1 . length) ys ]
  walkTill _ [] = Nothing
  walkTill uniques (x:xs)
    | x `elem` uniques = Just x
    | otherwise = walkTill uniques xs
