#!/usr/bin/env bash

orig_file=$1 in_file=$2 out_file=$3 options_ghc=$4

cat > $out_file <<EOF
{-# LANGUAGE AllowAmbiguousTypes, ApplicativeDo, BangPatterns, BinaryLiterals, BlockArguments, ConstraintKinds, DataKinds, DefaultSignatures, DeriveAnyClass, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, DerivingVia, DisambiguateRecordFields, DoAndIfThenElse, DuplicateRecordFields, EmptyDataDecls, ExistentialQuantification, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, KindSignatures, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, OverloadedStrings, OverloadedLists, PackageImports, PartialTypeSignatures, PatternGuards, PatternSynonyms, PolyKinds, QuantifiedConstraints, QuasiQuotes, RankNTypes, RecordWildCards, RecursiveDo, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeFamilyDependencies, TypeOperators, TypeSynonymInstances, UndecidableInstances, UnicodeSyntax, ViewPatterns #-}
{-# OPTIONS_GHC $options_ghc #-}
{-# LINE 1 "$orig_file" #-}
EOF

cat $in_file >> $out_file
