module Hix.Managed.Handlers.StateFile where

import Path (Abs, Dir, Path)

import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr)

data StateFileHandlers =
  StateFileHandlers {
    writeFile :: Maybe (Path Abs Dir) -> Expr -> M ()
  }

handlersNull :: StateFileHandlers
handlersNull =
  StateFileHandlers {writeFile = \ _ _ -> unit}
