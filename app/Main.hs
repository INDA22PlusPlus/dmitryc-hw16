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
reverse_list old_list new_list = reverse_list (tail old_list) ((head old_list):new_list)

main :: IO ()
--main = print(fib 20)
--main = print(fib_tail 10 0 1)
main = print(reverse_list [1..20] [])
