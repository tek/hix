module Hix.CabalParsec where

import Distribution.Parsec (Parsec, eitherParsec)
import Exon (exon)
import Type.Reflection (typeRep)

import Hix.Data.Error (Error (Client))
import Hix.Data.Monad (M)
import Hix.Monad (throwM)

eitherParsecFor ::
  Parsec b =>
  ToString a =>
  String ->
  a ->
  Either String b
eitherParsecFor desc spec =
  first errorMessage (eitherParsec s)
  where
    errorMessage err = error [exon|parsing #{desc} "#{s}": #{err}|]
    s = toString spec

unsafeParsecFor ::
  Parsec b =>
  ToString a =>
  String ->
  a ->
  b
unsafeParsecFor desc spec =
  either error id (eitherParsecFor desc spec)

unsafeParsec ::
  ∀ a b .
  Parsec b =>
  Typeable b =>
  ToString a =>
  a ->
  b
unsafeParsec =
  unsafeParsecFor (show (typeRep @b))

parsecM ::
  Parsec b =>
  ToString a =>
  String ->
  a ->
  M b
parsecM desc spec =
  leftA (throwM . Client . toText) (eitherParsecFor desc spec)
