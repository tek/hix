module Hix.Integration.Utils (
  module Hix.Integration.Utils,
  module Hix.Test.Utils,
) where

import Exon (exon)
import Path (Dir, File, Path, Rel, reldir, (</>))
import System.Environment (lookupEnv)

import Hix.Data.Monad (M)
import Hix.Error (pathText)
import Hix.Managed.Git (GitNative (..))
import Hix.Test.Utils

add :: GitNative -> Path Rel File -> Text -> M ()
add git path content = do
  addFile git.repo path content
  git.cmd_ ["add", pathText path]

local1 :: Path Rel Dir
local1 = [reldir|packages/local1|]

addP :: GitNative -> Path Rel File -> Text -> M ()
addP git path = add git (local1 </> path)

libHs :: Text
libHs = [exon|module Lib where|]

withHixDir :: (Text -> UnitTest) -> UnitTest
withHixDir main = do
  liftIO (lookupEnv "hix_dir") >>= \case
    Nothing -> unit
    Just hixRoot -> main (toText hixRoot)
