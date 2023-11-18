module Hix.Managed.Handlers.StateFile where

import Path (Abs, Dir, File, Path, Rel, (</>))

import Hix.Monad (M)
import Hix.NixExpr (Expr)

data StateFileHandlers =
  StateFileHandlers {
    initFile :: Path Abs Dir -> Path Rel File -> M (Path Abs File),
    writeFile :: Path Abs File -> Expr -> M ()
  }

handlersNull :: StateFileHandlers
handlersNull =
  StateFileHandlers {
    initFile = \ r f -> pure (r </> f),
    writeFile = \ _ _ -> unit
  }
