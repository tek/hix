module Hix.Data.ProjectFile where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import qualified Data.Text.IO as Text
import Path (File, Path, Rel, parent, toFilePath, (</>))
import Path.IO (createDirIfMissing)

import qualified Hix.Data.Monad (Env (cwd))
import Hix.Data.Monad (Env (Env), M)
import Hix.Error (tryIO)

data ProjectFile =
  ProjectFile {
    path :: Path Rel File,
    content :: Text
  }
  deriving stock (Eq, Show, Generic)

createFile :: ProjectFile -> M ()
createFile f = do
  Env {cwd} <- ask
  let
    file = cwd </> f.path
  lift $ tryIO do
    createDirIfMissing True (parent file)
    Text.writeFile (toFilePath file) f.content
