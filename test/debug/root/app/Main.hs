module Main where

import GHC.IO.Encoding (getFileSystemEncoding, getLocaleEncoding, setFileSystemEncoding, setLocaleEncoding, utf8)

main :: IO ()
main = do
  print =<< getLocaleEncoding
  print =<< getFileSystemEncoding
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  print =<< getLocaleEncoding
  print =<< getFileSystemEncoding
  putStrLn "📦"
