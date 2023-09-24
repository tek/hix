module Hix.Bootstrap where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.List.NonEmpty ((<|))
import qualified Data.Text as Text
import Distribution.Compiler (PerCompilerFlavor (PerCompilerFlavor))
import qualified Distribution.PackageDescription as Cabal
import Distribution.PackageDescription (
  BuildInfo,
  GenericPackageDescription,
  PackageDescription,
  UnqualComponentName,
  buildType,
  licenseFiles,
  unPackageName,
  unUnqualComponentName,
  )
import Distribution.Pretty (Pretty, pretty)
import Distribution.Simple (Dependency (Dependency), depVerRange)
import Distribution.Types.PackageDescription (license)
import Distribution.Utils.ShortText (ShortText, fromShortText)
import qualified Distribution.Verbosity as Cabal
import Exon (exon)
import Path (Abs, Dir, File, Path, Rel, parent, parseRelFile, relfile, toFilePath, (</>))
import System.FilePattern.Directory (getDirectoryFilesIgnore)

import Hix.Compat (readGenericPackageDescription)
import qualified Hix.Data.BootstrapProjectConfig
import Hix.Data.BootstrapProjectConfig (BootstrapProjectConfig)
import qualified Hix.Data.ComponentConfig
import Hix.Data.ComponentConfig (PackageName (PackageName))
import Hix.Data.Error (pathText, tryIO)
import qualified Hix.Data.NewProjectConfig
import qualified Hix.Data.ProjectFile
import Hix.Data.ProjectFile (ProjectFile (ProjectFile), createFile)
import qualified Hix.Monad
import Hix.Monad (Env (Env), M, noteBootstrap)
import qualified Hix.Prelude
import Hix.Prelude (Prelude, findPrelude)

data ExprAttr =
  ExprAttr {
    name :: Text,
    value :: Expr
  }
  |
  ExprAttrNil
  deriving stock (Eq, Show, Generic)

data Expr =
  ExprString Text
  |
  ExprLit Text
  |
  ExprList [Expr]
  |
  ExprAttrs [ExprAttr]
  |
  ExprPrefix Text Expr
  deriving stock (Eq, Show, Generic)

exprStrings :: [Text] -> Expr
exprStrings =
  ExprList . fmap ExprString

data CabalInfo =
  CabalInfo {
    path :: Path Rel Dir,
    info :: GenericPackageDescription
  }
  deriving stock (Eq, Show, Generic)

data ComponentType =
  Library
  |
  Executable Text
  |
  Benchmark Text
  |
  Test Text
  deriving stock (Eq, Show, Generic)

data PreludeWithVersion =
  PreludeWithVersion {
    prelude :: Prelude,
    dep :: Maybe Dependency
  }
  deriving stock (Eq, Show, Generic)

data HixComponent =
  HixComponent {
    special :: ComponentType,
    known :: [ExprAttr],
    prelude :: Maybe PreludeWithVersion
  }
  deriving stock (Eq, Show, Generic)

data HixPackage =
  HixPackage {
    name :: PackageName,
    src :: Path Rel Dir,
    known :: [ExprAttr],
    meta :: [ExprAttr],
    description :: ExprAttr,
    components :: [HixComponent]
  }
  deriving stock (Eq, Show, Generic)

indent ::
  Functor t =>
  Int ->
  t Text ->
  t Text
indent n =
  fmap (Text.replicate n " " <>)

withSemicolon :: NonEmpty Text -> NonEmpty Text
withSemicolon = \case
  e :| [] ->
    [e <> ";"]
  h :| h1 : t -> h <| withSemicolon (h1 :| t)

renderAttrs :: Int -> [ExprAttr] -> [Text]
renderAttrs ind attrs =
  attrs >>= \case
    ExprAttr k v ->
      case renderExpr ind v of
        e :| [] -> [[exon|#{k} = #{e};|]]
        h :| (h1 : t) -> [exon|#{k} = #{h}|] : toList (withSemicolon (h1 :| t))
    ExprAttrNil ->
      []

renderExpr :: Int -> Expr -> NonEmpty Text
renderExpr ind = \case
  ExprString s -> indent ind [[exon|"#{Text.replace "\"" "\\\"" s}"|]]
  ExprLit e -> [e]
  ExprList l -> "[" :| (indent (ind + 2) (toList . renderExpr ind =<< l)) ++ ["]"]
  ExprAttrs a -> case renderAttrs ind a of
    [] -> ["{};"]
    as -> "{" :| indent (ind + 2) as ++ ["}"]
  ExprPrefix p (renderExpr ind -> h :| t) ->
    [exon|#{p} #{h}|] :| t

renderRootExpr :: Expr -> Text
renderRootExpr =
  Text.unlines . toList . renderExpr 0

readCabal ::
  Path Abs Dir ->
  Path Rel File ->
  M CabalInfo
readCabal cwd path = do
  info <- liftIO (readGenericPackageDescription Cabal.verbose (toFilePath (cwd </> path)))
  pure CabalInfo {path = dir, info}
  where
    dir = parent path

class RenderCabalOption a where
  renderCabalOption :: a -> Text

instance {-# overlappable #-} Pretty a => RenderCabalOption a where
  renderCabalOption = show . pretty

instance RenderCabalOption ShortText where
  renderCabalOption = toText . fromShortText

instance RenderCabalOption String where
  renderCabalOption = toText

checkEmpty ::
  Text ->
  Expr ->
  ExprAttr
checkEmpty key = \case
  ExprString value | Text.null value ->
    ExprAttrNil
  ExprList value | null value ->
    ExprAttrNil
  value ->
    ExprAttr key value

singleOpt ::
  RenderCabalOption a =>
  Text ->
  (e -> Maybe a) ->
  e ->
  ExprAttr
singleOpt key get entity =
  maybe ExprAttrNil (checkEmpty key . ExprString) (renderCabalOption <$> get entity)

single ::
  RenderCabalOption a =>
  Text ->
  (e -> a) ->
  e ->
  ExprAttr
single key get =
  singleOpt key (Just . get)

multiOpt ::
  RenderCabalOption a =>
  Text ->
  (e -> Maybe [a]) ->
  e ->
  ExprAttr
multiOpt key get entity =
  maybe ExprAttrNil (checkEmpty key . exprStrings) (fmap renderCabalOption <$> get entity)

multi ::
  RenderCabalOption a =>
  Text ->
  (e -> [a]) ->
  e ->
  ExprAttr
multi key get =
  multiOpt key (Just . get)

multiOrSingle ::
  ∀ a e .
  RenderCabalOption a =>
  Text ->
  (e -> [a]) ->
  e ->
  ExprAttr
multiOrSingle key get entity =
  check (renderCabalOption <$> get entity)
  where
    check :: [Text] -> ExprAttr
    check [] = ExprAttrNil
    check [sing] = ExprAttr key (ExprString sing)
    check values = ExprAttr key (exprStrings values)

mkAttrs :: [e -> ExprAttr] -> e -> [ExprAttr]
mkAttrs a e =
  (fmap ($ e) a)

-- TODO extract version and put it in a file
knownPackageKeys :: PackageDescription -> [ExprAttr]
knownPackageKeys =
  mkAttrs [
    single "author" (.author),
    single "build-type" buildType,
    single "copyright" (.copyright),
    single "license" license,
    singleOpt "license-file" (head . licenseFiles),
    single "version" (.package.pkgVersion)
  ]

metaPackageKeys :: PackageDescription -> [ExprAttr]
metaPackageKeys =
  mkAttrs [
    single "maintainer" (.maintainer),
    single "homepage" (.homepage),
    single "synopsis" (.synopsis)
  ]

ghcFlavour :: PerCompilerFlavor a -> a
ghcFlavour (PerCompilerFlavor a _) = a

notDefaultGhcOption :: String -> Bool
notDefaultGhcOption = \case
  "-threaded" -> False
  "-rtsopts" -> False
  "-with-rtsopts=-N" -> False
  _ -> True

knownComponentKeys :: Maybe Prelude -> BuildInfo -> (Maybe PreludeWithVersion, [ExprAttr])
knownComponentKeys prelude info =
  (preludeWithVersion, vals)
  where
    vals =
      mkAttrs [
        multi "dependencies" (const deps),
        multi "default-extensions" (.defaultExtensions),
        multiOrSingle "source-dirs" (.hsSourceDirs),
        singleOpt "language" (.defaultLanguage),
        multi "ghc-options" (filter notDefaultGhcOption . ghcFlavour . (.options))
      ] info
    (preludeWithVersion, deps)
      | Just p <- prelude =
        let (v, res) = foldl (amendPrelude p) (Nothing, []) info.targetBuildDepends
        in (Just (PreludeWithVersion p v), res)
      | otherwise =
        (Nothing, filter notBase (info.targetBuildDepends))
    amendPrelude p (Nothing, ds) dep@(Dependency (Cabal.unPackageName -> dname) _ _) | dname == p.preludePackage =
      (Just dep, ds)
    amendPrelude _ (v, ds) d = (v, d : ds)

notBase :: Cabal.Dependency -> Bool
notBase = \case
  Cabal.Dependency "base" _ _ -> False
  _ -> True

convertComponent :: ComponentType -> BuildInfo -> HixComponent
convertComponent special info =
  HixComponent {..}
  where
    (prelude, known) = knownComponentKeys preludeBasic info
    preludeBasic = findPrelude info.mixins

convertLibrary :: Cabal.Library -> HixComponent
convertLibrary lib =
  convertComponent Library lib.libBuildInfo

convertExecutable :: UnqualComponentName -> Cabal.Executable -> HixComponent
convertExecutable name exe =
  convertComponent (Executable (toText (unUnqualComponentName name))) exe.buildInfo

convertTestsuite :: UnqualComponentName -> Cabal.TestSuite -> HixComponent
convertTestsuite name test =
  convertComponent (Test (toText (unUnqualComponentName name))) test.testBuildInfo

convertBenchmark :: UnqualComponentName -> Cabal.Benchmark -> HixComponent
convertBenchmark name bench =
  convertComponent (Benchmark (toText (unUnqualComponentName name))) bench.benchmarkBuildInfo

convert :: CabalInfo -> HixPackage
convert cinfo =
  HixPackage {
    name = PackageName (toText (unPackageName pkg.package.pkgName)),
    src = cinfo.path,
    known = knownPackageKeys pkg,
    meta = metaPackageKeys pkg,
    description = single "description" (.description) pkg,
    components
  }
  where
    components =
      maybeToList (convertLibrary . (.condTreeData) <$> info.condLibrary)
      <>
      (uncurry convertExecutable . second (.condTreeData) <$> info.condExecutables)
      <>
      (uncurry convertTestsuite . second (.condTreeData) <$> info.condTestSuites)
      <>
      (uncurry convertBenchmark . second (.condTreeData) <$> info.condBenchmarks)
    pkg = info.packageDescription
    info = cinfo.info

renderComponent :: HixComponent -> ExprAttr
renderComponent HixComponent {..} =
  ExprAttr key cabalConfig
  where
    cabalConfig = ExprAttrs (enable <> foldMap preludeAttrs prelude <> known)
    preludeAttrs p =
      [ExprAttr "prelude" (ExprAttrs [
        ExprAttr "package" (preludePackageAttrs p),
        ExprAttr "module" (ExprString (toText p.prelude.preludeModule))
      ])]
    preludePackageAttrs p
      | Just dep <- p.dep =
        ExprAttrs [
          (ExprAttr "name" (ExprString (toText p.prelude.preludePackage))),
          (ExprAttr "version" (ExprString (show (pretty (depVerRange dep)))))
        ]
      | otherwise = ExprString (toText p.prelude.preludePackage)
    key = case special of
      Library -> "library"
      Executable name -> [exon|executables.#{name}|]
      Test name -> [exon|tests.#{name}|]
      Benchmark name -> [exon|benchmarks.#{name}|]
    enable = case special of
      Library -> [ExprAttr "enable" (ExprLit "true")]
      _ -> []

flakePackage :: HixPackage -> ExprAttr
flakePackage pkg =
  ExprAttr name attrs
  where
    attrs = ExprAttrs (src : pkg.description : (ExprAttr "cabal" cabalConfig : comps))
    name = pkg.name.unPackageName
    src = ExprAttr "src" (ExprLit [exon|./#{Text.dropWhileEnd ('/' ==) (pathText pkg.src)}|])
    cabalConfig = ExprAttrs (pkg.known <> (if null pkg.meta then [] else [ExprAttr "meta" (ExprAttrs pkg.meta)]))
    comps = renderComponent <$> pkg.components

mainPackage :: [HixPackage] -> ExprAttr
mainPackage = \case
  pkg : _ : _ -> ExprAttr "main" (ExprString pkg.name.unPackageName)
  _ -> ExprAttrNil

flake :: BootstrapProjectConfig -> [HixPackage] -> Expr
flake conf pkgs =
  ExprAttrs [
    (ExprAttr "description" (ExprString "A Haskell project")),
    (ExprAttr "inputs.hix.url" (ExprString conf.hixUrl.unHixUrl)),
    (ExprAttr "outputs" (ExprPrefix "{hix, ...}: hix.lib.flake" (ExprAttrs [
      (ExprAttr "packages" (ExprAttrs (flakePackage <$> pkgs))),
      mainPackage pkgs
    ])))
  ]

bootstrapFiles :: BootstrapProjectConfig -> M [ProjectFile]
bootstrapFiles conf = do
  Env {cwd} <- ask
  cabals <- paths =<< lift (tryIO (getDirectoryFilesIgnore (toFilePath cwd) ["**/*.cabal"] ["dist-newstyle/**"]))
  pkgs <- fmap convert <$> traverse (readCabal cwd) cabals
  pure [
    ProjectFile {path = [relfile|flake.nix|], content = renderRootExpr (flake conf pkgs)}
    ]
  where
    paths = traverse (noteBootstrap "File path error" . parseRelFile)

bootstrapProject :: BootstrapProjectConfig -> M ()
bootstrapProject conf =
  traverse_ createFile =<< bootstrapFiles conf
