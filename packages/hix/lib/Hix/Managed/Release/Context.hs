module Hix.Managed.Release.Context where

import Hix.Data.Monad (M)
import Hix.Managed.Data.ReleaseContext (ReleaseContext (..), ReleaseContextProto (..))
import Hix.Path (resolvePathSpec)

validate :: ReleaseContextProto -> M ReleaseContext
validate ReleaseContextProto {hooks = hooksProto, ..} = do
  hooks <- traverse resolvePathSpec hooksProto
  pure ReleaseContext {..}
