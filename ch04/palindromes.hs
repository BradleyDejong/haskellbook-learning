
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x


myAbs :: (Num a, Ord a) => a -> a
myAbs x = if x > 0
  then x
  else x * (-1)


weirdTupleThing :: (a,b) -> (c,d) -> ((b,d), (a,c))
weirdTupleThing x y = ((snd x, snd y), (fst x, fst y))
