{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Utils where

import Nominal
import Nominal.Atom
import Nominal.Atomic


data V

instance AtomKind V where
  suggested_names _ = ["a", "b", "c", "d", "e", "x", "y", "z"]
  expand_names _ xs = xs ++ [ x ++ (show n) | n <- [1..], x <- xs ]

data Variable = Variable (AtomOfKind V) (NoBind String)
  deriving (Nominal.Generic, Bindable, Nominal, NominalShow, NominalSupport, Ord)

instance Show Variable where
  show (Variable a _) = show a

instance NominalShow (NoBind String) where
  showsPrecSup sup d (NoBind x) = showsPrecSup sup d x

instance Eq Variable where
  (Variable x _) == (Variable y _) = x == y
