module Main (main) where

import Lib ()
import Data.List

-- Declaring functions here because lib's not working
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib x = fib (x - 1) + fib (x - 2)

fib_tail :: Int -> Int -> Int -> Int
fib_tail 0 a _ = a
fib_tail 1 _ b = b
--fib_tail n 1 1 = fib_tail (n - 1) 1 2
fib_tail n a b = fib_tail (n - 1) b (a + b)

reverse_list :: [a] -> [a] -> [a]
reverse_list [] new_list = new_list
reverse_list old_list new_list = reverse_list (tail old_list) ((head old_list) : new_list)

-- Probably the most un-haskell code ever created, but i don't have time to fix it
convert_strings_to_lengths :: [String] -> [Int] -> [Int]
convert_strings_to_lengths [] ints = ints
convert_strings_to_lengths strings ints = convert_strings_to_lengths (tail strings) (length (head strings) : ints)

get_median_ints :: [Int] -> Float
get_median_ints ints = do
    let med_idx = div (length (ints)) 2
    if mod (length (ints)) 2 == 0
      then (fromIntegral (ints!!med_idx - 1) + fromIntegral(ints!!(med_idx))) / 2
      else fromIntegral (ints!!med_idx)

get_median :: [String] -> Float
get_median strings = get_median_ints (sort (convert_strings_to_lengths (strings) []))

-- 11 elements list
list_11 = ["icky", "panoramic", "new", "actually", "spotless", "town", "ready", "wrathful", "legs", "temporary", "brave"]

-- 12 elements list
list_12 = ["icky", "panoramic", "new", "actually", "spotless", "town", "ready", "wrathful", "legs", "temporary", "brave", "closed"]



main :: IO ()
-- Fibonacci testing
--main = print(fib 20)
--main = print(fib_tail 10 0 1)

-- Reverse testing
--main = print(reverse_list [1..20] [])

-- List median testing
--main = print(sort (convert_strings_to_lengths list_11 []))
--main = print(sort (convert_strings_to_lengths list_12 []))

main = do
    print(sort (convert_strings_to_lengths list_11 []))
    print(get_median list_11)

    print(sort (convert_strings_to_lengths list_12 []))
    print(get_median list_12)


-- Kattis: Aaah! https://open.kattis.com/problems/aaah?editsubmit=10557924
--readInput = (map length) . words 
--
--solve :: [Int] -> String
--solve (a:b:rest) = if a >= b
--                      then "go"
--                      else "no"
--
--main = interact (solve . readInput)
