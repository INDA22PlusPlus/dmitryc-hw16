module Main (main) where

import Lib

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib x = fib (x-1) + fib (x-2)

main :: IO ()
main = print(fib 20)
