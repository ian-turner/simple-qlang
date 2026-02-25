{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveAnyClass #-}

module Syntax where

import Nominal
import Nominal.Atom
import Nominal.Atomic

import Utils


data Exp
  = Unit                                      -- Unit type instance
  | NumInt Int                                -- Numbers
  | NumFloat Float
  | Var Variable                              -- Bound variables
  | Const String                              -- Top-level defined constant
  | Base String                               -- Compiler-defined constant
  | Tuple [Exp]                               -- Tuple of objects (any size)
  | App Exp Exp                               -- Function application
  | Lam (Bind [Variable] Exp)                 -- Bound lambda expression
  | Let Exp (Bind Variable Exp)               -- Bound let expressions
  | LetTuple Exp (Bind [Variable] Exp)
  | IfExp Exp Exp Exp                         -- If/then/else branching
  | Dynlift                                   -- Dynamic lifting
  deriving (Eq, Generic, Nominal,
    NominalShow, NominalSupport, Show)

data Decl
  = Def String Exp
  deriving (Show, Eq)
