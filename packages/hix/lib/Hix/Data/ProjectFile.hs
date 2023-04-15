module Hix.Data.ProjectFile where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import qualified Data.Text.IO as Text
import Path (File, Path, Rel, parent, toFilePath, (</>))
import Path.IO (createDirIfMissing)

import Hix.Data.Error (tryIO)
import qualified Hix.Monad
import Hix.Monad (Env (Env), M)

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
