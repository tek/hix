module Hix.Managed.Build.NixOutput where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (runStateT), modify', state)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, Value (String), withObject, (.:), (.:?))
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Distribution.Compat.CharParsing (
  CharParsing (char, notChar, string),
  Parsing (notFollowedBy, try),
  digit,
  letter,
  lower,
  )
import Distribution.Parsec (Parsec, eitherParsec, parsec)
import Exon (exon)

import Hix.Data.Json (jsonEither)
import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId as PackageId
import Hix.Data.PackageId (PackageId (..))
import qualified Hix.Log as Log
import Hix.Pretty (showP)

data Derivation =
  Derivation {
    path :: Text,
    log :: (Seq Text, Seq Text)
  }
  deriving stock (Eq, Show, Generic)

data PackageDerivation =
  PackageDerivation {
    package :: PackageId,
    success :: Bool,
    log :: [Text]
  }
  deriving stock (Eq, Show, Generic)

data BuildsState =
  BuildsState {
    id :: Integer,
    done :: Int,
    failed :: Int,
    unassigned :: [Bool]
  }
  deriving stock (Eq, Show, Generic)

data OutputState =
  OutputState {
    builds :: Maybe BuildsState,
    running :: Map Integer Derivation,
    finished :: [PackageDerivation]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

data OutputResult =
  OutputResult {
    failedPackages :: Maybe (NonEmpty PackageDerivation)
  }
  deriving stock (Eq, Show, Generic)

outputResult :: OutputState -> OutputResult
outputResult OutputState {finished} =
  OutputResult {failedPackages = nonEmpty (filter (not . (.success)) finished)}

runOutputState ::
  Monad m =>
  StateT OutputState m a ->
  m (a, OutputResult)
runOutputState ma =
  second outputResult <$> runStateT ma def

data NixAction =
  NixResult { aid :: Integer, rtype :: Int, fields :: [Either Text Int] }
  |
  NixStartBuilds Integer
  |
  NixStart Integer Text
  |
  NixStop Integer
  |
  NixStartOther Integer
  |
  NixMessage
  deriving stock (Eq, Show, Generic)

instance FromJSON NixAction where
  parseJSON =
    withObject "NixOutput" \ o ->
      o .: "action" >>= \case
        "result" -> do
          aid <- o .: "id"
          rtype <- o .: "type"
          fields <- fmap jsonEither <$> o .: "fields"
          pure NixResult {..}
        "start" -> do
          i <- o .: "id"
          o .:? "type" >>= \case
            Just (105 :: Int) ->
              o .: "fields" >>= \case
                String path : _ -> pure (NixStart i path)
                _ -> pure (NixStartOther i)
            Just 104 ->
              pure (NixStartBuilds i)
            _ -> pure (NixStartOther i)
        "stop" -> do
          i <- o .: "id"
          pure (NixStop i)
        "msg" -> pure NixMessage
        (act :: Text) -> fail [exon|Unknown action: #{toString act}|]

parseError :: String -> StateT s M ()
parseError err =
  lift $ Log.debug [exon|Nix output message parse error: #{toText err}|]

newtype StorePathName =
  StorePathName String
  deriving stock (Eq, Show, Generic)

instance Parsec StorePathName where
  parsec = do
    string "/nix/store/"
    some (lower <|> digit)
    char '-'
    pid0 <- some (notChar '.')
    pid <- some (notChar '.' <|> (try (char '.' <* notFollowedBy letter)))
    string ".drv"
    pure (StorePathName (pid0 <> pid))

addLogMessage :: Text -> Derivation -> Derivation
addLogMessage message Derivation {log = (current, prev), ..} =
  Derivation {log = updated, ..}
  where
    updated | Seq.length current > 100 = (pure message, current)
            | otherwise = (current |> message, prev)

finish :: Bool -> Derivation -> Either String PackageDerivation
finish success Derivation {path, log = (current, prev)} = do
  StorePathName name <- eitherParsec (toString path)
  package <- PackageId.fromCabal <$> eitherParsec name
  pure PackageDerivation {package, success, log = toList (prev <> current)}

tryFinish ::
  Maybe BuildsState ->
  Maybe Derivation ->
  (Maybe (Either Derivation PackageDerivation), [PackageDerivation], Maybe BuildsState)
tryFinish builds = \case
  Nothing -> (Nothing, [], builds)
  Just drv ->
    case finish success drv of
      Right pkg -> (Just (Right pkg), [pkg], newBuilds)
      Left _ -> (Just (Left drv), [], newBuilds)
  where
    (success, newBuilds) = case builds of
      Just s@BuildsState {unassigned = h : t} ->
        (h, Just s {unassigned = t})
      _ ->
        (False, builds)

reportFinished :: Either Derivation PackageDerivation -> StateT OutputState M ()
reportFinished result =
  lift $ Log.trace [exon|Nix build of #{desc} #{status}|]
  where
    (desc, status) = case result of
      Right PackageDerivation {package, success} -> (showP package, if success then "succeeded" else "failed")
      Left Derivation {path} -> (path, "finished with unknown status")

updateBuilds :: [Either Text Int] -> OutputState -> OutputState
updateBuilds [Right updatedDone, _, _, Right updatedFailed] s@OutputState {builds = Just bs@BuildsState {done, failed, unassigned}}
  | newDone > 0
  , newFailed == 0
  = s {builds = Just newBs {unassigned = unassigned ++ replicate newDone True}}
  | newFailed > 0
  , newDone == 0
  = s {builds = Just newBs {unassigned = unassigned ++ replicate newFailed False}}
  where
    newBs = bs {done = updatedDone, failed = updatedFailed}
    newDone = updatedDone - done
    newFailed = updatedFailed - failed
updateBuilds _ s = s

processResult :: Integer -> Int -> [Either Text Int] -> OutputState -> OutputState
processResult aid rtype fields s
  | rtype == 105
  , Just BuildsState {id = buildsId} <- s.builds
  , aid == buildsId
  = updateBuilds fields s
  | rtype == 101
  , Left message : _ <- fields
  = s {running = Map.adjust (addLogMessage message) aid s.running}
  | otherwise
  = s

processMessage ::
  ByteString ->
  NixAction ->
  StateT OutputState M ()
processMessage _ = \case
  NixResult {aid, rtype, fields} ->
    modify' (processResult aid rtype fields)

  NixStartBuilds i ->
    modify' \ s -> s {builds = Just BuildsState {id = i, done = 0, failed = 0, unassigned = []}}

  NixStart i path -> do
    lift $ Log.trace [exon|Started build of #{path} (#{show i})|]
    modify' \ OutputState {running, ..} ->
      OutputState {running = Map.insert i (Derivation path mempty) running, ..}

  NixStop i -> do
    result <- state \ OutputState {running, ..} -> do
      let (result, package, newBuilds) = tryFinish builds (running !? i)
      (result, OutputState {running = Map.delete i running, finished = finished ++ package, builds = newBuilds, ..})
    traverse_ reportFinished result

  NixStartOther _ -> unit

  NixMessage -> unit

outputParse :: ByteString -> StateT OutputState M ()
outputParse outputLine
  | Just payload <- ByteString.stripPrefix "@nix " outputLine
  = either parseError (processMessage payload) (Aeson.eitherDecodeStrict' payload)
  | otherwise
  = lift (Log.debug (decodeUtf8 outputLine))
