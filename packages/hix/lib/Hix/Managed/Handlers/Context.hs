{-# language CPP #-}

#if __GLASGOW_HASKELL__ >= 908
{-# language TypeAbstractions #-}
#endif

module Hix.Managed.Handlers.Context where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON)
import Distribution.Pretty (Pretty (pretty))
import Exon (exon)
import Text.PrettyPrint (hang, text, (<+>))

import qualified Hix.Color as Color
import Hix.Data.Json (JsonConfig)
import Hix.Data.Monad (AppResources (..), M, appRes)
import Hix.Json (jsonConfig)
import qualified Hix.Log as Log
import Hix.Managed.Data.MaintContext (MaintContext)
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto)
import Hix.Managed.Flake (runFlakeForSingleLine)
import Hix.Monad (appContext, eitherFatal, noteFatal, tryIOM)
import Hix.Pretty (HPretty (hpretty), showP)

type ContextKey :: Symbol -> Type -> Type
data ContextKey name a where
  ContextMaint :: ContextKey "maint" MaintContext
  ContextManaged :: ContextKey "managed" ProjectContextProto

instance KnownSymbol name => Pretty (ContextKey name a) where
  pretty _ = text (symbolVal (Proxy @name))

type ContextQuery :: Type -> Type
data ContextQuery a where
  ContextQuery :: ∀ name a . (FromJSON a, KnownSymbol name) => ContextKey name a -> ContextQuery a

contextKeySlug ::
  ∀ a b .
  IsString b =>
  ContextQuery a ->
  b
contextKeySlug (ContextQuery @name _) =
  fromString (symbolVal (Proxy @name))

instance Pretty (ContextQuery a) where
  pretty = text . contextKeySlug

data ContextHandlers =
  ContextHandlers {
    query :: ∀ a . ContextQuery a -> M a
  }

queryContextNoLog ::
  FromJSON a =>
  KnownSymbol name =>
  ContextHandlers ->
  ContextKey name a ->
  M a
queryContextNoLog ContextHandlers {query} key =
  query (ContextQuery key)

queryContext ::
  HPretty a =>
  FromJSON a =>
  KnownSymbol name =>
  ContextHandlers ->
  ContextKey name a ->
  M a
queryContext handlers key = do
  result <- queryContextNoLog handlers key
  Log.debugP $ hang (text "Context for" <+> pretty key) 2 (hpretty result)
  pure result

internalScope :: IsString a => a
internalScope = "__hix-internal__"

queryFlake :: ContextQuery a -> M a
queryFlake key@ContextQuery {} = do
  appContext [exon|querying the flake for the context #{Color.cyan name}|] do
    root <- appRes.root
    file <- decodeUtf8 <$> runFlakeForSingleLine desc root args id
    eitherFatal . first toText =<< tryIOM (Aeson.eitherDecodeFileStrict' file)
  where
    args = ["build", "--print-out-paths", [exon|.#{"#"}#{internalScope}.cli-context.json.#{name}|]]

    desc = [exon|Context query for '#{name}'|]

    name = contextKeySlug key

handlersProd :: ContextHandlers
handlersProd = ContextHandlers {query = queryFlake}

handlersTest :: (∀ a . ContextQuery a -> M (Maybe a)) -> ContextHandlers
handlersTest testContext =
  ContextHandlers {
    query = \ key -> check key =<< testContext key
  }
  where
    check key = noteFatal [exon|The context '#{showP key}' is not defined for this test.|]

jsonOrQuery ::
  FromJSON a =>
  KnownSymbol name =>
  ContextHandlers ->
  ContextKey name a ->
  Either a (Maybe JsonConfig) ->
  M a
jsonOrQuery ContextHandlers {query} confQuery =
  either pure (maybe (query (ContextQuery confQuery)) jsonConfig)

jsonOrQueryProd ::
  FromJSON a =>
  KnownSymbol name =>
  ContextKey name a ->
  Either a (Maybe JsonConfig) ->
  M a
jsonOrQueryProd =
  jsonOrQuery handlersProd
