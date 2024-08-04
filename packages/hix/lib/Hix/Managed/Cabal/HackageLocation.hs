module Hix.Managed.Cabal.HackageLocation where

import Data.List (dropWhileEnd, isInfixOf)
import Exon (Exon, exon)
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI)

import Hix.Managed.Cabal.Data.HackageLocation (
  HackageHost (..),
  HackageLocation (..),
  HackagePort (..),
  HackageTls (..),
  hackageTls,
  hackageTlsScheme,
  parseHackagePort,
  )
import Hix.Pretty (showP)

renderScheme ::
  Exon a =>
  IsString a =>
  HackageLocation ->
  a
renderScheme HackageLocation {tls} = [exon|#{hackageTlsScheme tls}:|]

renderPort ::
  Exon a =>
  IsString a =>
  HackageLocation ->
  a
renderPort HackageLocation {port}
  | Just p <- port = [exon|:##{showP p}|]
  | otherwise = ""

-- | No auth, omit default port.
-- TODO add a test with a hackage server that requires basic auth for everything (e.g. when querying with
-- nix-prefetch-url)
renderMinimal ::
  Exon a =>
  IsString a =>
  HackageLocation ->
  a
renderMinimal loc@HackageLocation {..} =
  [exon|#{renderScheme loc}//##{coerce @_ @Text host}#{renderPort loc}|]

hackageLocationUri :: HackageLocation -> URI
hackageLocationUri loc@HackageLocation {..} =
  URI {
    uriScheme = renderScheme loc,
    uriAuthority = Just URIAuth {
      uriPort = renderPort loc,
      uriRegName = toString (coerce @HackageHost @Text host),
      uriUserInfo = ""
    },
    uriPath = "/",
    uriQuery = "",
    uriFragment = ""
  }

hackagePort :: HackageLocation -> HackagePort
hackagePort HackageLocation {tls, port}
  | Just p <- port = p
  | TlsOn <- tls = 443
  | otherwise = 80

locationFromUri :: String -> URIAuth -> Either String HackageLocation
locationFromUri scheme URIAuth {..}
  | let stripped = dropWhileEnd ('@' ==) uriUserInfo
  , not (null stripped)
  = Left "User fragment not supported - please use the explicit option 'user'"
  | otherwise
  = do
  port <- if null uriPort
          then pure Nothing
          else maybeToRight [exon|Invalid port segment '#{uriPort}'|] (Just <$> parseHackagePort (drop 1 uriPort))
  pure HackageLocation {
    host = fromString uriRegName,
    tls = hackageTls (scheme == "https:"),
    port,
    auth = Nothing
  }

noSchemeMessage :: String -> String
noSchemeMessage spec =
  [exon|The Hackage URL '#{spec}' is missing a scheme, like 'https://'|]

parseLocation :: String -> Either String HackageLocation
parseLocation spec = do
  (uri, uriAuth) <- maybeToRight uriError do
    uri <- parseAbsoluteURI spec
    uriAuth <- uriAuthority uri
    pure (uri, uriAuth)
  first badLocation (locationFromUri (uriScheme uri) uriAuth)
  where
    uriError | hasScheme = [exon|Parse error in Hackage URL: #{spec}|]
             | otherwise = noSchemeMessage spec

    hasScheme = isInfixOf "://" spec

    badLocation err = [exon|Bad hackage repo location: #{err}|]
