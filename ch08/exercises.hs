
sumTo :: (Ord a, Eq a, Num a) => a -> a
sumTo n
  | n < 1 = 0
  | otherwise = go n 1 0
    where
      go to curr s
        | curr == to = s + to
        | otherwise =
            go to (curr + 1) (s + curr)
