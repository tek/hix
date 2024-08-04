module Hix.Managed.Data.Constraints where

import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (VersionRange)
import GHC.Exts (IsList)
import Text.PrettyPrint (Doc, parens, (<+>))

import Hix.Class.Map (LookupMaybe, NMap, nPretty)
import Hix.Data.PackageName (PackageName)
import Hix.Data.VersionBounds (VersionBounds, anyBounds, inclusiveRange)
import Hix.Pretty (prettyL)

-- | Left-biased semigroup op.
--
-- TODO it might be sensible to split the non-mutation deps out of this type so it can be keyed by @MutableDep@.
data MutationConstraints =
  MutationConstraints {
    -- | This used to be an abstraction, but there was no use case for it.
    -- Now it is hardcoded to use inclusive bounds for both, so a potential new mutation handler cannot customize that.
    -- All handlers only evolve one of the bounds, while this now allows both to be changed.
    mutation :: VersionBounds,

    -- | Prefer the oldest possible version.
    oldest :: Maybe Bool,

    -- | Prefer the installed version.
    installed :: Maybe Bool,

    -- | Concrete bounds overrides from user config.
    force :: Maybe VersionRange,

    -- | Prefer a concrete version range.
    prefer :: Maybe VersionRange
  }
  deriving stock (Eq, Show, Generic)

instance Pretty MutationConstraints where
  pretty MutationConstraints {mutation, oldest, installed, force, prefer} =
    pretty (inclusiveRange mutation) <+>
    flags [("oldest", oldest), ("installed", installed)] <+>
    foldMap pref prefer <+>
    foldMap spec force
    where
      flags :: [(Doc, Maybe Bool)] -> Doc
      flags =
        mapMaybe flag >>> \case
          [] -> mempty
          fs -> parens (prettyL fs)

      flag = \case
        (desc, Just True) -> Just desc
        _ -> Nothing

      pref v = "~" <+> pretty v
      spec v = "+" <+> pretty v

-- TODO maybe try creating something lawful, or at least intuitive?
instance Semigroup MutationConstraints where
  l <> r =
    MutationConstraints {
      mutation = l.mutation <> l.mutation,
      oldest = l.oldest <|> r.oldest,
      installed = l.installed <|> r.installed,
      force = l.force <|> r.force,
      prefer = l.prefer <|> r.prefer
    }

instance Monoid MutationConstraints where
  mempty =
    MutationConstraints {
      mutation = anyBounds,
      oldest = Nothing,
      installed = Nothing,
      force = Nothing,
      prefer = Nothing
    }

newtype EnvConstraints =
  EnvConstraints (Map PackageName MutationConstraints)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Monoid, IsList)

instance Semigroup EnvConstraints where
  EnvConstraints l <> EnvConstraints r = EnvConstraints (Map.unionWith (<>) l r)

instance NMap EnvConstraints PackageName MutationConstraints LookupMaybe where

instance Pretty EnvConstraints where
  pretty = nPretty
