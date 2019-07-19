module Lib
    ( someFunc
    ) where

import System.Environment

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0    = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0    = []
  | otherwise = [x `mod` 10] ++ toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:xs) = [x] ++ [y*2] ++ doubleEveryOther xs
doubleEveryOther [] = []
doubleEveryOther [x] = [x]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumDigit x + sumDigits xs

sumDigit :: Integer -> Integer
sumDigit 0 = 0
sumDigit x = sumDigit (x `div` 10) + (x `mod` 10)

validate :: Integer -> Bool
validate x
  | x `mod` 10 == 0 = True
  | otherwise       = False


someFunc :: IO ()
someFunc = do
  args <- getArgs
  let x = read (args !! 0) :: Integer
  let digList = toDigits x
  let digListRev = toDigitsRev x
  putStrLn $ show digList
  putStrLn $ show digListRev
  putStrLn $ show (doubleEveryOther digList)
  putStrLn $ show (doubleEveryOther digListRev)
  putStrLn $ show (sumDigit 123)
  putStrLn $ show (sumDigits digList)
  putStrLn $ show (sumDigits (doubleEveryOther digList))
  putStrLn $ show (sumDigits (doubleEveryOther digListRev))
  putStrLn $ show (validate (sumDigits (doubleEveryOther digListRev)))
