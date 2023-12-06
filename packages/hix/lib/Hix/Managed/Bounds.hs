module Hix.Managed.Bounds where

import Hix.Class.Map (ntAmend)
import Hix.Data.Bounds (Bounds)
import Hix.Data.Version (Versions)
import Hix.Version (setLowerBound)

setLowerBounds ::
  Versions ->
  Bounds ->
  Bounds
setLowerBounds =
  ntAmend setLowerBound
