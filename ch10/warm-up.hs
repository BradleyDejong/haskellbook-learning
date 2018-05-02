module WarmUp where

import Control.Applicative (liftA3)

stops :: [Char]
stops  = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

nouns :: [String]
nouns = ["a baker", "a butcher", "a candlestick maker", "the moon"]

verbs :: [String]
verbs = ["jumps over", "slides under", "falls in love with"]

combinations :: [a] -> [a] -> [(a, a, a)]
combinations a b =
  liftA3 (\x y z -> (x,y,z)) a b a
  -- (fmap (\x y z -> (x,y,z)) a) <*> b <*> a
  -- ((\x y z -> (x,y,z)) <$> a) <*> b <*> a

  
  

apCombinations :: [Char] -> [Char] -> [(Char, Char, Char)]
apCombinations x y =
  filter pred $ combinations x y
  where
    pred :: (Char, Char, Char) -> Bool
    pred ('p', _, _) = True
    pred (_,_,_) = False


seekritFunc x =
  div (sum (map length (words x))) (length (words x))

seekritFuncFrac x =
  (/) (fromIntegral $ sum (map length (words x))) (fromIntegral $ length (words x))
