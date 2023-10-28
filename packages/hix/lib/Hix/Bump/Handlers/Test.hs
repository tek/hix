module Hix.Bump.Handlers.Test where

import Distribution.Version (Version, mkVersion)
import Exon (exon)

import Hix.Bump.Handlers.Prod (prodHandlers)
import Hix.Data.BumpHandlers (BumpHandlers (..))
import Hix.Data.ComponentConfig (PackageName)
import Hix.Data.Error (Error (BumpError))
import Hix.Monad (M, throwM)

latestVersion :: PackageName -> M (Maybe Version)
latestVersion = \case
  "aeson" -> found [2, 2, 0, 0]
  "extra" -> found [1, 7, 14]
  "th-abstraction" -> found [0, 5, 0, 0]
  n -> throwM (BumpError [exon|Invalid package for latestVersion: ##{n}|])
  where
    found = pure . Just . mkVersion

testHandlers :: IO BumpHandlers
testHandlers = do
  h <- prodHandlers
  pure h {latestVersion}
