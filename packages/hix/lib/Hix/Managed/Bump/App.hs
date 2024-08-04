module Hix.Managed.Bump.App where

import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (BumpOptions, ManagedOptions)
import Hix.Managed.App (managedApp)
import Hix.Managed.Bump.Optimize (bumpOptimizeMain)
import Hix.Managed.Data.ProjectResult (ProjectResult)
import Hix.Managed.Handlers.Context (ContextKey (ContextManaged), jsonOrQueryProd)
import Hix.Managed.ProjectContext (processProjectResult)

bumpApp :: ManagedOptions -> M ProjectResult
bumpApp opts = do
  context <- jsonOrQueryProd ContextManaged opts.context
  managedApp opts context bumpOptimizeMain

bumpCli :: BumpOptions -> M ()
bumpCli opts = processProjectResult =<< bumpApp opts.common
