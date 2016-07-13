{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module L1(module Syntax
         ,ExpF(..)
         ,ExpF1
         ,L1
         ,ExpF2
         ,L2
         ,Prim(..)) where

import Syntax

-- base functor (two-level types trick)
data ExpF t e =
  Op Operator [e]
  | If e e e
  | App e [e]
  | Lam Args e t
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
  | Let (Binds e t) e
  | Letrec (Binds e t) e
  | As e t
  | Begin [e] e
  | Repeat Name e e e -- int int e
  | P Prim

data Prim =
  Var Name
  | TimerStart
  | TimerStop
  | TimerReport
  | ReadInt
  | N Integer
  | B Bool
  | Unit
  deriving (Eq)

-- newtype Exp = Exp (Fix ExpF)

type ExpF1 = ExpF Type
type L1 = L ExpF1
type ExpF2 = ExpF ([Type],Type)
type L2 = L ExpF2
