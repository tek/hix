module Hix where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Hix.Data.Error (Error (..), printPreprocError)
import Hix.Options (Command (Preproc), GlobalOptions (..), Options (Options), parseCli)
import Hix.Preproc (preprocess)

handleError ::
  MonadIO m =>
  GlobalOptions ->
  Error ->
  m ()
handleError GlobalOptions {verbose} = \case
  PreprocError err -> printPreprocError err
  NoMatch msg | (fromMaybe False verbose) -> printPreprocError msg
  NoMatch _ -> unit

runCommand :: Command -> ExceptT Error IO ()
runCommand = \case
  Preproc opts -> preprocess opts

main :: IO ()
main = do
  Options global cmd <- parseCli
  leftA (handleError global) =<< runExceptT (runCommand cmd)
