module AToThe where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe = foldl appender "" . map valueOrA . map notThe . words
  where valueOrA :: Maybe String -> String
        valueOrA Nothing = "a"
        valueOrA (Just value) = value

        appender :: String -> String -> String
        appender x "" = x
        appender "" y = y
        appender x y = x ++ " " ++ y




-- Utility function
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x




-- Exercise 12.5.2
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go 0 . words
  where go :: Integer -> [String] -> Integer
        go i (x:y:ys)
          | x == "the" && isVowel (safeHead y) = go (i + 1) (y:ys)
          | otherwise = go i (y:ys)
        go i _ = i

        isVowel :: Maybe Char -> Bool
        isVowel Nothing = False
        isVowel (Just x) = myElem x "aeiou"

myElem :: (Foldable t, Eq a) => a -> t a -> Bool
myElem val xs = foldr (\x acc -> acc || x == val) False xs


vowels = "aeiou"

countVowels :: String -> Integer
countVowels = foldr (\_ y -> y + 1) 0 . filter ((flip elem) vowels)



flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs =
  foldr theFolder (Just []) xs

  where theFolder :: Maybe a -> Maybe [a] -> Maybe [a]
        theFolder Nothing _ = Nothing
        theFolder _ Nothing = Nothing
        theFolder (Just val) (Just acc) = Just (val:acc)
  

flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
  flipMaybe (x:xs) = map (\val -> (val:flipMaybe
                                  
      
