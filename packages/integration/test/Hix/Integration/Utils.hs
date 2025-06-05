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

local2 :: Path Rel Dir
local2 = [reldir|packages/local2|]

local3 :: Path Rel Dir
local3 = [reldir|packages/local3|]

addP :: GitNative -> Path Rel File -> Text -> M ()
addP git path = add git (local1 </> path)

addP2 :: GitNative -> Path Rel File -> Text -> M ()
addP2 git path = add git (local2 </> path)

addP3 :: GitNative -> Path Rel File -> Text -> M ()
addP3 git path = add git (local3 </> path)

libHs :: Text
libHs = [exon|module Lib where|]

lib1Hs :: Text
lib1Hs = [exon|module Lib1 where|]

lib2Hs :: Text
lib2Hs = [exon|module Lib2 where|]

lib3Hs :: Text
lib3Hs = [exon|module Lib3 where|]

withHixDir :: (Text -> UnitTest) -> UnitTest
withHixDir main = do
  liftIO (lookupEnv "hix_dir") >>= \case
    Nothing -> unit
    Just hixRoot -> main (toText hixRoot)
