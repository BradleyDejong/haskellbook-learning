module Exercises where

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf needle@(x:xs) (y:haystack) =
  if x == y
  then isSubseqOf xs haystack
  else isSubseqOf needle haystack


