module Hix.Data.Monad (
  module Hix.Data.Monad,
  module Hix.Data.LogLevel,
) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.Generics.Labels ()
import GHC.Records (HasField (getField))
import Path (Abs, Dir, Path)

import Hix.Data.AppContext (AppContext)
import Hix.Data.Error (Error)
import Hix.Data.LogLevel (LogLevel (..))
import Hix.Data.OutputFormat (OutputFormat)
import Hix.Data.OutputTarget (OutputTarget)

data AppResources =
  AppResources {
    cwd :: Path Abs Dir,
    tmp :: Path Abs Dir,
    root :: Path Abs Dir,
    logLevel :: LogLevel,
    cabalVerbose :: Bool,
    output :: OutputFormat,
    target :: OutputTarget,
    logger :: LogLevel -> Text -> M (),
    context :: [AppContext]
  }
  deriving stock (Generic)

newtype M a =
  M (ReaderT AppResources (ExceptT Error IO) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

liftE :: ExceptT Error IO a -> M a
liftE = M . lift

data AppResProxy = AppResProxy

instance HasField name AppResources a => HasField name AppResProxy (M a) where
  getField AppResProxy = M (asks (getField @name))

appRes :: AppResProxy
appRes = AppResProxy
