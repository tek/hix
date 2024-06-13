module Hix.Managed.Build.NixOutput.Analysis where

import Distribution.Parsec (simpleParsec)
import Distribution.Simple (Dependency)

import qualified Hix.Data.Dep as Dep
import Hix.Data.Dep (Dep)

analyzeLog :: [Text] -> Maybe (NonEmpty Dep)
analyzeLog log =
  nonEmpty bounds
  where
    bounds = catMaybes (takeWhile isJust (parseDep <$> afterBoundsMarker))
    afterBoundsMarker = drop 1 (dropWhile (boundsMarker /=) log)
    boundsMarker = "Error: Setup: Encountered missing or private dependencies:"
    parseDep = fmap Dep.fromCabal . simpleParsec @Dependency . toString
