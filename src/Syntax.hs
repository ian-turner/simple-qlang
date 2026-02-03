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
  = Unit
  | Num Integer
  | Var Variable
  | Tuple [Exp]
  | App Exp Exp
  | Lam (Bind [Variable] Exp)
  | Let (Bind [Variable] Exp)
  | IfExp Exp Exp Exp
  deriving (Eq, Generic, Nominal, NominalShow, NominalSupport, Show)
