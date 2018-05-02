module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 2
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items =
  foldr datesOnlyPls [] items
  where
    datesOnlyPls (DbDate d) y = d : y
    datesOnlyPls _ y = y

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber items = foldr numbersOnlyPls [] items
  where
    numbersOnlyPls (DbNumber n) y = n : y
    numbersOnlyPls _ y = y


mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Integer
avgDb items = sumDb items `div` (fromIntegral $ length $ filterDbNumber items)
