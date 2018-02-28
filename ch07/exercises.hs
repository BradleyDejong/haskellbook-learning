
tensDigit :: Integral a => a -> a
tensDigit x = d
  where toTens = fst $ divMod x 10
        d      = snd $ divMod toTens 10


foldBool :: a -> a -> Bool -> a
foldBool x y b
  | b = y
  | otherwise = x


g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = ((f a), c)

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
  print $ ((roundTrip 4) :: Int)
  print (id 4)
