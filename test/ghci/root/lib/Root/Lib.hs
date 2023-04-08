module Root.Lib where

import Control.Exception
import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Network.HTTP.Client

tryRequest :: Manager -> IO (Response ByteString.ByteString)
tryRequest man =
  spin 10
  where
    spin 0 = req
    spin n = catch req (err n)
    err :: Int -> SomeException -> IO (Response ByteString.ByteString)
    err n _ = do
      threadDelay 1000000
      spin (n - 1)
    req =
      httpLbs "http://localhost:20002/test" man

test :: IO ()
test = do
  man <- newManager defaultManagerSettings
  res <- tryRequest man
  ByteString.putStrLn (responseBody res)
