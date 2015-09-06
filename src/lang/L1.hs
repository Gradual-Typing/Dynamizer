{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module L1(module Syntax
         ,L1
         ,Exp(..)
         ,Bind) where

import Syntax

type Bind = (Name,Type,L1)
type Binds = [Bind]

type L1 = L Exp

data Exp =
  N Integer
  | B Bool
  | Unit
  | Op Operator [L1]
  | If L1 L1 L1
  | Var Name
  | App L1 [L1]
  | Lam Args L1 Type
  | GRef L1
  | GDeRef L1
  | GAssign L1 L1
  | MRef L1
  | MDeRef L1
  | MAssign L1 L1
  | GVect L1 L1 -- length value
  | GVectRef L1 L1 -- vect pos
  | GVectSet L1 L1 L1 -- vect pos value
  | MVect L1 L1
  | MVectRef L1 L1
  | MVectSet L1 L1 L1
  | Let Binds L1
  | Letrec Binds L1
  | As L1 Type
  | Begin [L1] L1
  | Repeat Name L1 L1 L1 -- int int e
  | TimerStart | TimerStop | TimerReport
  deriving (Eq)
