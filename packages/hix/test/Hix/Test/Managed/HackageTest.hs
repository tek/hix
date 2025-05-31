module Hix.Test.Managed.HackageTest where

import Exon (exon)
import Hedgehog ((===))
import Test.Tasty (TestTree, testGroup)

import Hix.CabalParsec (unsafeParsec)
import Hix.Class.EncodeNix (encodeNix)
import Hix.Managed.Cabal.Data.ContextHackageRepo (
  ContextHackageLocation (..),
  ContextHackagePassword (..),
  ContextHackageRepo (..),
  ContextHackageSecret (..),
  )
import Hix.Managed.Cabal.Data.HackageLocation (HackageLocation (..), HackageTls (TlsOff), hackageLocation)
import Hix.Managed.Cabal.HackageLocation (noSchemeMessage, parseLocation)
import Hix.NixExpr (renderRootExpr)
import Hix.Test.Utils (UnitTest, unitTest)

test_hackageRepo :: UnitTest
test_hackageRepo = do
  Left (noSchemeMessage "server.com") === parseLocation "server.com"
  Right ((hackageLocation "server.com" TlsOff) {port = Just 10}) === parseLocation "http://server.com:10"

target_encodeNix_ContextHackageRepo :: Text
target_encodeNix_ContextHackageRepo =
  [exon|{
  name = "test";
  description = "test";
  enable = false;
  location = "http://localhost:1234";
  user = "test";
  password = "test";
  secure = false;
  keys = [
    "key1"
    "key2"
  ];
  indexState = "2024-01-01T00:00:00Z";
  solver = true;
  publish = true;
}
|]

contextRepo :: ContextHackageRepo
contextRepo =
  ContextHackageRepo {
    name = "test",
    description = Just "test",
    enable = Just False,
    location = Just (ContextHackageLocation "http://localhost:1234"),
    user = Just "test",
    password = Just (ContextHackagePassword (SecretUnobscured "test")),
    token = Nothing,
    secure = Just False,
    keys = Just ["key1", "key2"],
    indexState = Just (unsafeParsec ("2024-01-01T00:00:00Z" :: String)),
    solver = Just True,
    publish = Just True
  }

test_encodeNix_ContextHackageRepo :: UnitTest
test_encodeNix_ContextHackageRepo =
  target_encodeNix_ContextHackageRepo === renderRootExpr (encodeNix contextRepo)

test_hackageData :: TestTree
test_hackageData =
  testGroup "hackage data" [
    unitTest "parse HackageRepo" test_hackageRepo,
    unitTest "nix-encode ContextHackageRepo" test_encodeNix_ContextHackageRepo
  ]
