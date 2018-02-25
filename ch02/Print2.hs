module Print2 where

main :: IO()
main = do
  putStrLn "Count to four:"
  putStrLn "  One, two"
  putStrLn "     three, four!"


data Mood = Blah | Woot deriving Show
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

isPalendrome :: Eq a => [a] -> Bool
isPalendrome x = x == reverse x

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10






f :: Bool -> Maybe Int
f True = Nothing
f False = Just 0

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
  

mcCarthy91 :: Integer -> Integer
mcCarthy91 x
  | x < 100 = 91
  | otherwise = x - 10



mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs


bZip :: [a] -> [b] -> [(a,b)]
bZip _ [] = []
bZip [] _ = []
bZip (x:xs) (y:ys) = (x,y) : bZip xs ys
