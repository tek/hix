module Hix.Managed.Cabal.HackageRepo where

import Exon (exon)

import qualified Hix.Managed.Cabal.Data.HackageLocation as HackageLocation
import Hix.Managed.Cabal.Data.HackageLocation (HackageLocation (HackageLocation, host), central)
import Hix.Managed.Cabal.Data.HackageRepo (HackageDescription (..), HackageName, HackageRepo (..), centralName)

hackageDescription :: HackageLocation -> HackageDescription
hackageDescription HackageLocation {host} =
  if host == central.host
  then "central Hackage"
  else HackageDescription [exon|Hackage at ##{host}|]

hackageRepo :: HackageName -> HackageLocation -> HackageRepo
hackageRepo name location =
  HackageRepo {
    name,
    description = hackageDescription location,
    location,
    enable = True,
    secure = Just True,
    keys = Nothing,
    solver = True,
    publish = False,
    indexState = Nothing
  }

centralHackage :: HackageRepo
centralHackage =
  (hackageRepo centralName HackageLocation.central) {
    description = "central Hackage",
    publish = True
  }
