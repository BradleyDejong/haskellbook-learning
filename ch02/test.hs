module Learn where

import Prelude

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple :: Int -> Int
triple = (*) 3

piTimes :: Double -> Double
piTimes = (*) pi

y = 20

x = y * 5

