module Hix.Test.Managed.GitPushTest where

import Hedgehog ((===))

import Hix.Managed.Git.Push (rejectedRefs)
import Hix.Test.Utils (UnitTest)

output :: [Text]
output =
  [
    "To https://github.com/user/pkg",
    "*\trefs/heads/release/pkg/1.7.3.0-1791732271:refs/heads/release/pkg/1.7.3.0-1791732271\t[new branch]",
    "!\trefs/heads/release/pkg/1.7.3.0:refs/heads/release/pkg/1.7.3.0\t[rejected] (fetch first)",
    "Done"
  ]

test_pushRejected :: UnitTest
test_pushRejected = do
  ["release/pkg/1.7.3.0"] === rejectedRefs output
  [] === rejectedRefs (take 2 output ++ ["Done"])
