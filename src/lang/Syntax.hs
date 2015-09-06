{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Syntax where

import Text.Parsec.Pos (SourcePos)

type L a = (SourcePos, a)

type Name = String
type Arg = (Name,Type)
type Args = [Arg]

data Operator = Plus | Minus | Mult | Div | Eq | Ge | Gt | Le | Lt
              | ShiftR | ShiftL | BAnd | BOr
                deriving (Eq)

data Type =
  Dyn
  | IntTy
  | BoolTy
  | UnitTy
  | FunTy [Type] Type
  | GRefTy Type
  | MRefTy Type
  | GVectTy Type
  | MVectTy Type
  deriving (Eq)
