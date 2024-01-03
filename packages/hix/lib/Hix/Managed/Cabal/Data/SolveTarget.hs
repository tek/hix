module Hix.Managed.Cabal.Data.SolveTarget where

import Distribution.Client.Dependency (PackagePreference (..), PackageSpecifier (NamedPackage, SpecificSourcePackage))
import Distribution.Client.Types (UnresolvedSourcePackage)
import Distribution.Pretty (Pretty (pretty))
import Distribution.Solver.Types.PackageConstraint (dispPackageProperty)
import Distribution.Version (simplifyVersionRange)
import Text.PrettyPrint (Doc, (<+>))

import Hix.Pretty (prettyL)

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
