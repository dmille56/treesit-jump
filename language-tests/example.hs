{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List
import qualified Data.Map as Map
import System.IO

-- Simple function definition
add :: Int -> Int -> Int
add x y = x + y

-- Using guards
factorial :: Integer -> Integer
factorial n
  | n == 0 = 1
  | n > 0 = n * factorial (n - 1)
  | otherwise = error "Negative input"

-- Pattern matching and recursion
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- List comprehension
squares :: [Int] -> [Int]
squares xs = [x ^ 2 | x <- xs]

-- Higher-order functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Currying and partial application
multiply :: Int -> Int -> Int
multiply x y = x * y

double = multiply 2

-- Map data structure
phoneBook :: Map.Map String String
phoneBook = Map.fromList [("alice", "703-493-1834"), ("bob", "857-384-1234")]

-- Monad example with Maybe
findKey :: String -> Maybe String
findKey key = Map.lookup key phoneBook

-- IO action
main :: IO ()
main = do
  putStrLn "Enter a number:"
  input <- getLine
  let number = read input :: Int
  putStrLn $ "The factorial of " ++ show number ++ " is " ++ show (factorial number)
  putStrLn $ "Fibonacci: " ++ show (fibonacci number)
  putStrLn $ "Squares: " ++ show (squares [1 .. number])
  putStrLn $ "Applying double twice to 5: " ++ show (applyTwice double 5)
  putStrLn $ "Looking up Alice in the phone book: " ++ show (findKey "alice")
