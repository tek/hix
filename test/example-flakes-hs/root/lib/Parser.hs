module Parser where

import Data.Aeson (encode, object, toJSON, (.=))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.String (fromString)
import System.Environment (getArgs)
import Text.Read (readMaybe)

createJson :: Int -> String
createJson n = unpack (encode (object [fromString "number" .= toJSON n]))

parseNumber :: IO String
parseNumber = parse <$> getArgs
  where
    parse (input : _) = maybe ("Not a number: " ++ input) createJson (readMaybe input)
    parse _ = "No argument given."
