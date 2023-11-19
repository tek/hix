module Hix.Class.SOP where

import Generics.SOP (I (I), NP (Nil, (:*)), NS (S, Z), SOP (SOP))
import Generics.SOP.GGP (GCode, GDatatypeInfoOf, GFrom, gfrom)
import Generics.SOP.Type.Metadata (ConstructorInfo (Record), DatatypeInfo (..), FieldInfo (FieldInfo))

data FieldK =
  FieldK {
    name :: Symbol,
    tpe :: Type
  }

type Field :: FieldK -> Type
data Field k where
  Field :: a -> Field ('FieldK name a)

type ProdFields :: [FieldInfo] -> [Type] -> [FieldK] -> Constraint
class ProdFields info as fields | info as -> fields where
  prodFields :: NP I as -> NP Field fields

instance ProdFields '[] '[] '[] where
  prodFields Nil = Nil

instance (
    ProdFields info as fields
  ) => ProdFields ('FieldInfo name : info) (a : as) ('FieldK name a : fields) where
    prodFields (I a :* fields) = Field a :* prodFields @info fields

type SumFields :: [ConstructorInfo] -> [[Type]] -> [[FieldK]] -> Constraint
class SumFields cons ass fields | cons ass -> fields where
  sumFields :: NS (NP I) ass -> NS (NP Field) fields

instance SumFields '[] '[] '[] where
  sumFields = \case

instance (
    ProdFields info as conFields,
    SumFields cons ass fields
  ) => SumFields ('Record con info : cons) (as : ass) (conFields : fields) where
    sumFields = \case
      Z con -> Z (prodFields @info con)
      S cons -> S (sumFields @cons cons)

class ToFields a fields | a -> fields where
  toFields :: a -> SOP Field fields

instance (
    Generic a,
    GFrom a,
    'ADT mod name cons sni ~ GDatatypeInfoOf a,
    ass ~ GCode a,
    SumFields cons ass fields
  ) => ToFields a fields where
    toFields (gfrom -> SOP fields) = SOP (sumFields @cons fields)
