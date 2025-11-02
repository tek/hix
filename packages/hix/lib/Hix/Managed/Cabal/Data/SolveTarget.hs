{-# language CPP #-}

module Hix.Managed.Cabal.Data.SolveTarget where

import Distribution.Client.Dependency (PackagePreference (..), PackageSpecifier (NamedPackage, SpecificSourcePackage))
import Distribution.Client.Types (UnresolvedSourcePackage)
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (simplifyVersionRange)
import Text.PrettyPrint (Doc, (<+>))

import Hix.Pretty (prettyL)

#if MIN_VERSION_cabal_install(3,16,0)

dispPackageProperty :: a -> a
dispPackageProperty = id

#else

import Distribution.Solver.Types.PackageConstraint (dispPackageProperty)

#endif

data SolveTarget =
  SolveTarget {
    dep :: PackageSpecifier UnresolvedSourcePackage,
    prefs :: [PackagePreference]
  }

prettySpec :: PackageSpecifier UnresolvedSourcePackage -> Doc
prettySpec = \case
  NamedPackage name prop ->
    "Named:" <+> pretty name <+> prettyL (dispPackageProperty <$> prop)
  SpecificSourcePackage pkg ->
    "Source:" <+> show pkg

prettyPref :: PackagePreference -> Doc
prettyPref = \case
  PackageVersionPreference   pn vr ->
    pretty pn <+> pretty (simplifyVersionRange vr)
  PackageInstalledPreference pn ip ->
    pretty pn <+> show ip
  PackageStanzasPreference pn st ->
    pretty pn <+> show st

instance Pretty SolveTarget where
  pretty SolveTarget {..} =
    prettySpec dep <+> prettyL (prettyPref <$> prefs)
