{-# language CPP #-}

module Hix.Bootstrap where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
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
import Distribution.Pretty (pretty)
import Distribution.Simple (Dependency (Dependency), depVerRange)
import Distribution.Types.PackageDescription (license)
import qualified Distribution.Verbosity as Cabal
import Exon (exon)
import Path (Abs, Dir, File, Path, Rel, parent, parseRelFile, relfile, toFilePath, (</>))
import System.FilePattern.Directory (getDirectoryFilesIgnore)

import Hix.Compat (readGenericPackageDescription)
import qualified Hix.Data.BootstrapProjectConfig
import Hix.Data.BootstrapProjectConfig (BootstrapProjectConfig)
import qualified Hix.Data.Monad (AppResources (cwd))
import Hix.Data.Monad (M (M))
import qualified Hix.Data.NewProjectConfig
import Hix.Data.NixExpr (Expr (ExprAttrs, ExprLit, ExprPrefix, ExprString), ExprAttr (ExprAttr, ExprAttrNil))
import Hix.Data.PackageName (PackageName (PackageName))
import qualified Hix.Data.ProjectFile
import Hix.Data.ProjectFile (ProjectFile (ProjectFile), createFile)
import Hix.Error (pathText, tryIO)
import Hix.Monad (AppResources (AppResources), noteBootstrap)
import Hix.NixExpr (mkAttrs, multi, multiOrSingle, nonEmptyAttrs, renderRootExpr, single, singleOpt)
import qualified Hix.Prelude
import Hix.Prelude (Prelude, findPrelude)

#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path (makeSymbolicPath)
#endif

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

readCabal ::
  Path Abs Dir ->
  Path Rel File ->
  M CabalInfo
readCabal cwd path = do
#if MIN_VERSION_Cabal(3,14,0)
  info <- liftIO (readGenericPackageDescription Cabal.verbose Nothing (makeSymbolicPath (toFilePath (cwd </> path))))
#else
  info <- liftIO (readGenericPackageDescription Cabal.verbose (toFilePath (cwd </> path)))
#endif
  pure CabalInfo {path = dir, info}
  where
    dir = parent path

-- TODO extract version and put it in a file
knownPackageKeys :: PackageDescription -> [ExprAttr]
knownPackageKeys =
  mkAttrs [
    single "author" (.author),
    single "build-type" (pretty . buildType),
    single "copyright" (.copyright),
    single "license" (pretty . license),
    singleOpt "license-file" (fmap pretty . head . licenseFiles),
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
        multi "dependencies" (fmap pretty . const deps),
        multi "default-extensions" (fmap pretty . (.defaultExtensions)),
        multiOrSingle "source-dirs" (fmap pretty . (.hsSourceDirs)),
        singleOpt "language" (fmap pretty . (.defaultLanguage)),
        multi "ghc-options" (fmap (ExprString . toText) . filter notDefaultGhcOption . ghcFlavour . (.options)),
        misc
      ] info

    misc e =
      nonEmptyAttrs "component" (mkAttrs [
        multi "other-modules" (fmap pretty . (.otherModules))
      ] e)

    (preludeWithVersion, deps)
      | Just p <- prelude =
        let (v, res) = foldl (amendPrelude p) (Nothing, []) info.targetBuildDepends
        in (Just (PreludeWithVersion p v), res)
      | otherwise =
        (Nothing, filter notBase info.targetBuildDepends)

    amendPrelude p (Nothing, ds) dep@(Dependency (Cabal.unPackageName -> dname) _ _) | dname == p.preludePackage =
      (Just dep, ds)
    amendPrelude _ (v, ds) d = (v, d : ds)

notBase :: Cabal.Dependency -> Bool
notBase = \case
  Cabal.Dependency "base" _ _ -> False
  _ -> True

convertComponent :: ComponentType -> BuildInfo -> [ExprAttr] -> HixComponent
convertComponent special info extra =
  HixComponent {known = knownCommon <> extra, ..}
  where
    (prelude, knownCommon) = knownComponentKeys preludeBasic info
    preludeBasic = findPrelude info.mixins

convertLibrary :: Cabal.Library -> HixComponent
convertLibrary lib =
  convertComponent Library lib.libBuildInfo extra
  where
    extra = mkAttrs [
      multi "reexported-modules" (fmap pretty . (.reexportedModules))
      ] lib

convertExecutable :: UnqualComponentName -> Cabal.Executable -> HixComponent
convertExecutable name exe =
  convertComponent (Executable (toText (unUnqualComponentName name))) exe.buildInfo []

convertTestsuite :: UnqualComponentName -> Cabal.TestSuite -> HixComponent
convertTestsuite name test =
  convertComponent (Test (toText (unUnqualComponentName name))) test.testBuildInfo []

convertBenchmark :: UnqualComponentName -> Cabal.Benchmark -> HixComponent
convertBenchmark name bench =
  convertComponent (Benchmark (toText (unUnqualComponentName name))) bench.benchmarkBuildInfo []

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
          ExprAttr "name" (ExprString (toText p.prelude.preludePackage)),
          ExprAttr "version" (ExprString (show (pretty (depVerRange dep))))
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
  ExprAttr (coerce pkg.name) attrs
  where
    attrs = ExprAttrs (src : pkg.description : ExprAttr "cabal" cabalConfig : comps)
    src = ExprAttr "src" (ExprLit [exon|./#{Text.dropWhileEnd ('/' ==) (pathText pkg.src)}|])
    cabalConfig = ExprAttrs (pkg.known <> [ExprAttr "meta" (ExprAttrs pkg.meta) | not (null pkg.meta)])
    comps = renderComponent <$> pkg.components

mainPackage :: [HixPackage] -> ExprAttr
mainPackage = \case
  pkg : _ : _ -> ExprAttr "main" (ExprString (coerce pkg.name))
  _ -> ExprAttrNil

flake :: BootstrapProjectConfig -> [HixPackage] -> Expr
flake conf pkgs =
  ExprAttrs [
    ExprAttr "description" (ExprString "A Haskell project"),
    ExprAttr "inputs.hix.url" (ExprString conf.hixUrl.unHixUrl),
    ExprAttr "outputs" (ExprPrefix "{hix, ...}: hix.lib.flake" (ExprAttrs [
      ExprAttr "packages" (ExprAttrs (flakePackage <$> pkgs)),
      mainPackage pkgs
    ]))
  ]

bootstrapFiles :: BootstrapProjectConfig -> M [ProjectFile]
bootstrapFiles conf = do
  AppResources {cwd} <- M ask
  cabals <- paths =<< M (lift (tryIO (getDirectoryFilesIgnore (toFilePath cwd) ["**/*.cabal"] ["dist-newstyle/**"])))
  pkgs <- fmap convert <$> traverse (readCabal cwd) cabals
  pure [
    ProjectFile {path = [relfile|flake.nix|], content = renderRootExpr (flake conf pkgs)}
    ]
  where
    paths = traverse (noteBootstrap "File path error" . parseRelFile)

bootstrapProject :: BootstrapProjectConfig -> M ()
bootstrapProject conf =
  traverse_ createFile =<< bootstrapFiles conf
