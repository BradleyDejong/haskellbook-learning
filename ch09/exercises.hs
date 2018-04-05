
myWords :: String -> [String]
myWords = splitter ((==) ' ')

myLines :: String -> [String]
myLines = splitter ((==) '\n')

splitter :: (a -> Bool) -> [a] -> [[a]]
splitter _ [] = []
splitter p input = first : remaining
  where
    first = takeWhile (not . p) input
    remaining = (splitter p) . (dropWhile p) . (dropWhile (not . p)) $ input
