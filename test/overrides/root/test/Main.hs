module Main where

import Data.Functor (void)
import System.Process.Typed (readProcess, proc)

main :: IO ()
main = void (readProcess (proc "git" []))
