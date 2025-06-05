module Hix.Managed.VersionIncrement where

import Distribution.Version (mkVersion, versionNumbers)

import Hix.Data.Version (Version)
import Hix.Managed.Data.VersionIncrement (VersionIncrement (..))

incrementVersion :: Version -> VersionIncrement -> Version
incrementVersion current spec =
  mkVersion (spin (versionNumbers current) (Just spec))
  where
    spin = \cases
      (n : rest) (Just Supermajor) -> (n + 1) : spin rest Nothing
      [] (Just Supermajor) -> 1 : spin [] Nothing
      (n : rest) (Just inc) -> n : spin rest (Just (succ inc))
      [] (Just inc) -> 0 : spin [] (Just (succ inc))
      (_ : rest) Nothing -> 0 : spin rest Nothing
      [] Nothing -> []
