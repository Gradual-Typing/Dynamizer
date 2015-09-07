{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Syntax where

import Text.Parsec.Pos (SourcePos)

type Name = String
type Arg = (Name,Type)
type Args = [Arg]
type Bind e = (Name,Type,e)
type UBind e = (Name,e)
type UBinds e = [UBind e]
type Binds e = [Bind e]

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

newtype Fix e = In {out::e (Fix e)}

-- data Ann a f x = Ann a (f x)
data Ann a e = Ann a (e (Ann a e))

type L a = Ann SourcePos a
