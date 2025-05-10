module Main where

import Parser (parseNumber)

main :: IO ()
main = putStrLn =<< parseNumber
