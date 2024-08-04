module Hix.Test.Managed.HackageTest where

import Hedgehog ((===))

import Hix.Managed.Cabal.Data.HackageLocation (HackageLocation (..), HackageTls (TlsOff), hackageLocation)
import Hix.Managed.Cabal.HackageLocation (noSchemeMessage, parseLocation)
import Hix.Test.Utils (UnitTest)

test_hackageRepo :: UnitTest
test_hackageRepo = do
  Left (noSchemeMessage "server.com") === parseLocation "server.com"
  Right ((hackageLocation "server.com" TlsOff) {port = Just 10}) === parseLocation "http://server.com:10"
