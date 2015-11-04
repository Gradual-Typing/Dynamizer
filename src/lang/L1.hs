{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module L1(module Syntax
         ,ExpF(..)
         ,L1) where

import Syntax

-- base functor (two-level types trick)
data ExpF e =
  N Integer
  | B Bool
  | Unit
  | Op Operator [e]
  | If e e e
  | Var Name
  | App e [e]
  | Lam Args e Type
  | GRef e
  | GDeRef e
  | GAssign e e
  | MRef e
  | MDeRef e
  | MAssign e e
  | GVect e e -- length value
  | GVectRef e e -- vect pos
  | GVectSet e e e -- vect pos value
  | MVect e e
  | MVectRef e e
  | MVectSet e e e
  | Let (Binds e) e
  | Letrec (Binds e) e
  | As e Type
  | Begin [e] e
  | Repeat Name e e e -- int int e
  | TimerStart | TimerStop | TimerReport
  deriving (Eq)

-- newtype Exp = Exp (Fix ExpF)
type L1 = L ExpF
