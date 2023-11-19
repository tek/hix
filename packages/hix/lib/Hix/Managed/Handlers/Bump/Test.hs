module Hix.Managed.Handlers.Bump.Test where

import Distribution.Version (Version, mkVersion)
import Exon (exon)

import Hix.Data.Error (Error (Client))
import Hix.Data.Package (PackageName)
import Hix.Managed.Handlers.Bump (BumpHandlers (..))
import Hix.Managed.Handlers.Bump.Prod (handlersProd)
import Hix.Monad (M, throwM)

latestVersion :: PackageName -> M (Maybe Version)
latestVersion = \case
  "aeson" -> found [2, 2, 0, 0]
  "extra" -> found [1, 7, 14]
  "th-abstraction" -> found [0, 5, 0, 0]
  n -> throwM (Client [exon|Invalid package for latestVersion: ##{n}|])
  where
    found = pure . Just . mkVersion

handlersTest :: IO BumpHandlers
handlersTest = do
  h <- handlersProd
  pure h {latestVersion}