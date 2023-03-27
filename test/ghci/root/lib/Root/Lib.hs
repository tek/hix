{-# language OverloadedStrings #-}

module Root.Lib where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Network.HTTP.Client

test :: IO ()
test = do
  man <- newManager defaultManagerSettings
  res <- httpLbs "http://localhost:22000/test" man
  ByteString.putStrLn (responseBody res)
