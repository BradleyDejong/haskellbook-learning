
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = b == aToB a


arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB i a = (aToB a) + fromIntegral i

