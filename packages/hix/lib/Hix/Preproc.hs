module Hix.Preproc where

import Control.Lens (IndexedTraversal', has, index, ix, preview, (%~), (.~), (^..))
import Control.Lens.Regex.ByteString (Match, group, groups, match, regex)
import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.ByteString as ByteString
import Data.ByteString (elemIndex)
import qualified Data.ByteString.Builder as ByteStringBuilder
import Data.ByteString.Builder (Builder, byteString, charUtf8, stringUtf8)
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Distribution.PackageDescription (BuildInfo (..))
import Distribution.Simple (PerCompilerFlavor (PerCompilerFlavor))
import qualified Exon
import Exon (exon)
import Language.Haskell.Extension (
  Extension (DisableExtension, EnableExtension, UnknownExtension),
  Language (UnknownLanguage),
  )
import Path (Abs, Dir, File, Path, SomeBase (Abs), toFilePath)
import Prelude hiding (group)
import System.Random (randomRIO)

import Hix.Cabal (buildInfoForFile)
import Hix.Component (targetComponentOrError)
import qualified Hix.Data.ComponentConfig
import Hix.Data.ComponentConfig (PreludeConfig, PreludePackage (PreludePackageName, PreludePackageSpec))
import Hix.Data.Json (JsonConfig)
import Hix.Data.Monad (LogLevel (LogVerbose), M, liftE)
import Hix.Data.Options (PreprocOptions (..), TargetSpec (TargetForFile))
import Hix.Data.PackageName (PackageName (PackageName))
import Hix.Data.PathSpec (PathSpec (PathConcrete))
import qualified Hix.Data.PreprocConfig
import Hix.Data.PreprocConfig (PreprocConfig)
import Hix.Error (Error (..), ErrorMessage (Client), sourceError)
import Hix.Json (jsonConfigE)
import Hix.Monad (tryIOM)
import Hix.Path (resolvePathSpecDir, resolvePathSpecFile)
import qualified Hix.Prelude as Prelude
import Hix.Prelude (Prelude (Prelude), findPrelude)

type Regex = IndexedTraversal' Int ByteString Match

-- TODO do we need to parse the spec here?
fromPreludeConfig :: PreprocConfig -> PreludeConfig -> Prelude
fromPreludeConfig ppconf conf =
  Prelude (toString name) (toString conf.module_.unModuleName) local
  where
    local = Map.member (PackageName name) ppconf.packages
    name = case conf.package of
      PreludePackageName n -> n
      PreludePackageSpec n -> n

data CabalConfig =
  CabalConfig {
    extensions :: [Builder],
    ghcOptions :: [Builder],
    prelude :: Maybe Prelude
  }
  deriving stock (Show, Generic)

newtype DummyExportName =
  DummyExportName { unDummyExportName :: ByteString }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

noMatch :: Text -> Path b File -> ExceptT Error IO a
noMatch reason source =
  throwE (Error {message = Client (sourceError reason source), context = [], level = Just LogVerbose})

takeLine :: ByteString -> Maybe (ByteString, ByteString)
takeLine bs =
  elemIndex 10 bs <&> \ i ->
    let (xs, ys) = ByteString.splitAt i bs
    in (xs, ByteString.tail ys)

nl :: Builder
nl = charUtf8 '\n'

lineB :: Builder -> Builder
lineB bs =
  bs <> nl

line :: ByteString -> Builder
line bs =
  lineB (byteString bs)

joinLinesReverse :: [ByteString] -> ByteString
joinLinesReverse =
  foldr joinLine mempty
  where
    joinLine a b = b <> a <> "\n"

joinLinesReverseBuilder :: [ByteString] -> Builder
joinLinesReverseBuilder =
  foldr joinLine mempty
  where
    joinLine a b = b <> line a

extension :: Extension -> Maybe Builder
extension = \case
  EnableExtension ext -> Just (show ext)
  DisableExtension ext -> Just ("No" <> show ext)
  UnknownExtension _ -> Nothing

languagePragma :: [Builder] -> Builder
languagePragma exts =
  [exon|{-# language #{Exon.intercalate ", " exts} #-}|]

extensionsPragma :: CabalConfig -> Maybe Builder
extensionsPragma conf
  | null conf.extensions = Nothing
  | otherwise = Just (languagePragma conf.extensions)

optionsPragma :: Builder -> Builder
optionsPragma opts =
  lineB [exon|{-# options_ghc #{opts} #-}|]

noImplicitPreludeRegex :: Regex
noImplicitPreludeRegex =
  [regex|\bNoImplicitPrelude\b|]

commentRegex :: Regex
commentRegex =
  [regex|^\s*--|]

moduleRegex :: Regex
moduleRegex =
  [regex|^\s*module\b\s+(\S+)|]

moduleEndRegex :: Regex
moduleEndRegex =
  [regex|\bwhere\b|]

importsEndRegex :: Regex
importsEndRegex =
  [regex|^\S|]

importRegex :: Regex
importRegex =
  [regex|^import\b|]

containsNoImplicitPrelude :: ByteString -> Bool
containsNoImplicitPrelude =
  has noImplicitPreludeRegex

isComment :: ByteString -> Bool
isComment =
  has commentRegex

isModule :: ByteString -> Maybe ByteString
isModule =
  preview (moduleRegex . group 0)

isModuleEnd :: ByteString -> Bool
isModuleEnd =
  has moduleEndRegex

isImportsEnd :: ByteString -> Bool
isImportsEnd =
  has importsEndRegex

isImport :: ByteString -> Bool
isImport =
  has importRegex

data Phase =
  PreModule
  |
  ModuleStart
  |
  ModuleExports
  |
  Imports
  deriving stock (Eq, Show, Generic)

data PreludeAction =
  PreludeDefault
  |
  PreludeNoImplicit
  |
  PreludeReplaced
  deriving stock (Eq, Show, Generic)

data CustomPrelude =
  CustomPrelude Prelude PreludeAction
  |
  NoCustomPrelude
  deriving stock (Show, Generic)

notPre :: Phase -> Bool
notPre = \case
  PreModule -> False
  _ -> True

pattern NotPre :: Phase
pattern NotPre <- (notPre -> True)

inModule :: Phase -> Bool
inModule = \case
  ModuleStart -> True
  ModuleExports -> True
  _ -> False

pattern InModule :: Phase
pattern InModule <- (inModule -> True)

preludeRegex :: Regex
preludeRegex =
  [regex|^import\s+(?:qualified\s+)?((?:"[^"]+"\s+)?)(Prelude\b)(?:$|[^.])|]

replacePrelude :: ByteString -> Prelude -> Maybe ByteString
replacePrelude l Prelude {..}
  | has preludeRegex l =
    Just (l & preludeRegex . groups %~ insertPrelude)
  | otherwise =
    Nothing
  where
    insertPrelude =
      addPackage
      .
      (ix 1 .~ encodeUtf8 preludeModule)
    addPackage | local = id
               | otherwise = ix 0 .~ [exon|"#{encodeUtf8 preludePackage}" |]

parenRegex :: Regex
parenRegex =
  [regex|(\()|]

dummyExportPlaceholder :: ByteString
dummyExportPlaceholder =
  "HIX_DUMMY_EXPORT>"

dummyExportPlaceholderRegex :: Regex
dummyExportPlaceholderRegex =
  [regex|HIX_DUMMY_EXPORT>|]

insertExport ::
  ByteString ->
  ByteString
insertExport =
  parenRegex . index 0 . match .~ ("(" <> dummyExportPlaceholder)

moduleExportsRegex :: Regex
moduleExportsRegex =
  [regex|\bmodule ([\w.]+)\s*($|,|--|\))|]

moduleExports ::
  ByteString ->
  [ByteString]
moduleExports l =
  l ^.. moduleExportsRegex . group 0

data Header =
  Header {
    moduleLines :: [ByteString],
    importLines :: [ByteString],
    rest :: Builder,
    moduleEndLine :: Int,
    importsEndLine :: Int,
    prelude :: CustomPrelude,
    exportsSelf :: Bool
  }
  deriving stock (Show, Generic)

data ScanState =
  ScanState {
    phase :: Phase,
    moduleLines :: [ByteString],
    importLines :: [ByteString],
    moduleLength :: Int,
    importsLength :: Int,
    prelude :: CustomPrelude,
    moduleName :: Maybe ByteString,
    exportsSelf :: Bool
  }
  deriving stock (Show, Generic)

scanHeader ::
  Maybe Prelude ->
  ByteString ->
  Header
scanHeader customPrelude =
  tryProcessLine ScanState {
    phase = PreModule,
    moduleLines = mempty,
    importLines = mempty,
    moduleLength = 1,
    importsLength = 0,
    prelude = initPrelude,
    moduleName = Nothing,
    exportsSelf = False
  }
  where
    initPrelude = case customPrelude of
      Just p -> CustomPrelude p PreludeDefault
      Nothing -> NoCustomPrelude

    tryProcessLine s input =
      case takeLine input of
        Just (nextLine, inputRest) ->
          processLine s nextLine inputRest
        Nothing ->
          finish s mempty

    processLine s l ls | isComment l || ByteString.isPrefixOf "#" l =
      pushCurrent s l ls
    processLine ScanState {phase = PreModule, prelude = CustomPrelude p PreludeDefault, ..} l ls | containsNoImplicitPrelude l =
      pushModule ScanState {phase = PreModule, prelude = CustomPrelude p PreludeNoImplicit, ..} l ls
    processLine s@ScanState {phase = PreModule} l ls | Just name <- isModule l =
      changePhase (s & #moduleName .~ Just name) ModuleStart l ls
    processLine s@ScanState {phase = ModuleStart} l ls | has parenRegex l =
      changePhase s ModuleExports (insertExport l) ls
    processLine s@ScanState {phase = ModuleExports, moduleName = Just name, exportsSelf = False} l ls
      | exs <- moduleExports l
      , elem name exs =
        processLine (s & #exportsSelf .~ True) l ls
    processLine ScanState {phase = InModule, ..} l ls | isModuleEnd l =
      pushModule ScanState {phase = Imports, ..} l ls
    processLine s@ScanState {phase = InModule} l ls | isImport l =
      changePhase s Imports l ls
    processLine ScanState {phase = Imports, prelude = CustomPrelude p action, ..} l ls | isImport l =
      let
        (replaced, newAction) =
          case replacePrelude l p of
            Just new -> (new, PreludeReplaced)
            Nothing -> (l, action)
      in pushImport ScanState {phase = Imports, prelude = CustomPrelude p newAction, ..} replaced ls
    processLine s@ScanState {phase = Imports} l ls | isImportsEnd l =
      finish s (line l <> byteString ls)
    processLine s l ls =
      pushCurrent s l ls

    changePhase s phase = processLine s {phase}

    pushCurrent s@ScanState {phase}
      | Imports <- phase = pushImport s
      | otherwise = pushModule s

    pushModule ScanState {..} l =
      tryProcessLine ScanState {
        moduleLines = l : moduleLines,
        moduleLength = moduleLength + 1,
        ..
      }

    pushImport ScanState {..} l =
      tryProcessLine ScanState {importLines = l : importLines, importsLength = importsLength + 1, ..}

    finish ScanState {..} rest =
      Header {
        moduleEndLine = moduleLength,
        importsEndLine = moduleLength + importsLength,
        ..
      }

customPreludeImport :: Prelude -> Builder
customPreludeImport Prelude {..} =
  lineB [exon|import#{package} #{stringUtf8 preludeModule} as Prelude|]
  where
    package | local = ""
            | otherwise = [exon| "#{stringUtf8 preludePackage}"|]

needPreludeExtensions :: PreludeAction -> Bool
needPreludeExtensions = \case
  PreludeDefault -> True
  PreludeNoImplicit -> False
  PreludeReplaced -> True

pattern NeedPreludeExtensions :: PreludeAction
pattern NeedPreludeExtensions <- (needPreludeExtensions -> True)

needDummy :: CustomPrelude -> Bool
needDummy = \case
  NoCustomPrelude -> False
  CustomPrelude _ action -> needPreludeExtensions action

pattern NeedDummy :: CustomPrelude
pattern NeedDummy <- (needDummy -> True)

preludeExtensions :: CustomPrelude -> Builder
preludeExtensions = \case
  CustomPrelude _ NeedPreludeExtensions ->
    lineB (languagePragma ["PackageImports", "NoImplicitPrelude"])
  _ ->
    mempty

explicitPreludeImport ::
  Builder ->
  CustomPrelude ->
  Builder
explicitPreludeImport lineNo = \case
  CustomPrelude prelude PreludeDefault -> customPreludeImport prelude <> lineNo
  _ -> mempty

dummyDecl ::
  CustomPrelude ->
  Builder ->
  DummyExportName ->
  Builder
dummyDecl NeedDummy lineNo (DummyExportName n) =
  lineB [exon|type #{byteString n} = Int|] <> lineNo
dummyDecl _ _ _ =
  mempty

replaceDummy ::
  CustomPrelude ->
  Bool ->
  DummyExportName ->
  ByteString ->
  ByteString
replaceDummy NeedDummy False (DummyExportName n) =
  dummyExportPlaceholderRegex . index 0 . match .~ [exon|#{n},|]
replaceDummy _ _ _ =
  dummyExportPlaceholderRegex . index 0 . match .~ ""

assemble ::
  Path Abs File ->
  Header ->
  Maybe Builder ->
  Maybe Builder ->
  DummyExportName ->
  Builder
assemble source Header {..} exts options dummyExportName =
  foldMap optionsPragma options <>
  foldMap lineB exts <>
  preludeExtensions prelude <>
  linePragma 1 <>
  byteString (replaceDummy prelude exportsSelf dummyExportName moduleString) <>
  explicitPreludeImport (linePragma moduleEndLine) prelude <>
  importsString <>
  dummyDecl prelude (linePragma importsEndLine) dummyExportName <>
  rest
  where
    linePragma n =
      lineB [exon|{-# line #{show n} "#{stringUtf8 (toFilePath source)}" #-}|]
    moduleString = joinLinesReverse moduleLines
    importsString = joinLinesReverseBuilder importLines

preprocessModule ::
  Path Abs File ->
  CabalConfig ->
  DummyExportName ->
  ByteString ->
  Builder
preprocessModule source conf dummyExportName inLines =
  assemble source header (extensionsPragma conf) options dummyExportName
  where
    options = Exon.intercalate " " <$> nonEmpty conf.ghcOptions
    header = scanHeader conf.prelude inLines

preprocessWith :: PreprocOptions -> CabalConfig -> M ()
preprocessWith opt conf = do
  inLines <- tryIOM . ByteString.readFile . toFilePath =<< resolvePathSpecFile opt.inFile
  dummyNumber :: Int <- randomRIO (10000, 10000000)
  let dummyExportName = DummyExportName [exon|Hix_Dummy_#{show dummyNumber}|]
  source <- resolvePathSpecFile opt.source
  let result = preprocessModule source conf dummyExportName inLines
  outFile <- resolvePathSpecFile opt.outFile
  tryIOM (ByteStringBuilder.writeFile (toFilePath outFile) result)

fromConfig ::
  Maybe (Path Abs Dir) ->
  Path Abs File ->
  Either PreprocConfig JsonConfig ->
  M CabalConfig
fromConfig cliRoot source pconf = do
  conf <- jsonConfigE pconf
  target <- targetComponentOrError cliRoot Nothing conf.packages (TargetForFile $ PathConcrete (Abs source))
  pure CabalConfig {
    extensions = stringUtf8 <$> target.component.language : target.component.extensions,
    ghcOptions = stringUtf8 <$> target.component.ghcOptions,
    prelude = fromPreludeConfig conf <$> target.component.prelude
    }

fromCabal :: BuildInfo -> CabalConfig
fromCabal info =
  CabalConfig {
    extensions = maybeToList (dlExtension =<< info.defaultLanguage) <> mapMaybe extension info.defaultExtensions,
    ghcOptions = stringUtf8 <$> ghcOptions,
    prelude = findPrelude info.mixins
    }
  where
    PerCompilerFlavor ghcOptions _ = info.options
    dlExtension = \case
      UnknownLanguage _ -> Nothing
      lang -> Just (stringUtf8 (show lang))

fromCabalFile :: Path Abs File -> M CabalConfig
fromCabalFile source =
  fromCabal <$> liftE (buildInfoForFile source)

-- TODO add common stanzas
preprocess :: PreprocOptions -> M ()
preprocess opt = do
  source <- resolvePathSpecFile opt.source
  root <- traverse resolvePathSpecDir opt.root
  conf <- maybe (fromCabalFile source) (fromConfig root source) opt.config
  preprocessWith opt conf
