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

-- | Type expressions (no variable binding, no Nominal instances needed)
data TypeExp
  = TyVar String                        -- Type variable (lowercase, e.g. a, b)
  | TyCon String                        -- Type constant (uppercase, e.g. Int, Qubit)
  | TyApp TypeExp TypeExp               -- Type application (e.g. Maybe a)
  | TyFun TypeExp TypeExp               -- Function type (e.g. a -> b)
  | TyTuple [TypeExp]                   -- Tuple type (e.g. (a, b))
  deriving (Show, Eq)

data Decl
  = Def String Exp
  | TypeSig String TypeExp              -- Type annotation (e.g. f : a -> b)
  deriving (Show, Eq)
