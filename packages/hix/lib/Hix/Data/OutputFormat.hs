module Hix.Data.OutputFormat where

data OutputFormat =
  OutputNone
  |
  OutputJson
  deriving stock (Eq, Show, Generic)
