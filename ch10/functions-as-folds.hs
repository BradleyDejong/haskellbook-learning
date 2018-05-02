
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False


myElem :: Eq a => a -> [a] -> Bool
myElem a = myAny ((==) a)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []


myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []

myMap' :: (a -> b) -> [a] -> [b]
myMap' f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x : y else y) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . (fmap f)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy f = foldr comp Nothing
  where
    comp a Nothing = Just a
    comp a (Just b) = if f a b == GT then Just a else Just b

myMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMinimumBy f = foldr comp Nothing
  where
    comp a Nothing = Just a
    comp a (Just b) = if f a b == LT then Just a else Just b
