module Hix.Data.OutputFormat where

data OutputFormat =
  OutputNone
  |
  OutputJson
  |
  OutputCommitMsg
  |
  OutputGaPr
  deriving stock (Eq, Show, Generic)
