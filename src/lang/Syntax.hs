{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Syntax where

import Text.Parsec.Pos (SourcePos)

type Name = String
--type Arg = (Name,Type)
type Args = [Name]
type Bind e = (Name,Type,e)
type Binds e = [Bind e]

data Operator = Plus | Minus | Mult | Div | Eq | Ge | Gt | Le | Lt
              | ShiftR | ShiftL | BAnd | BOr
                deriving (Eq,Show)

data Type =
  Dyn
  | IntTy
  | BoolTy
  | UnitTy
  | FunTy [Type] Type
  | ArrTy [Type] Type
  | GRefTy Type
  | MRefTy Type
  | GVectTy Type
  | MVectTy Type
  deriving (Eq,Show)

newtype Fix e = In {out::e (Fix e)}

-- data Ann a f x = Ann a (f x)
data Ann a e = Ann a (e (Ann a e))

type L a = Ann SourcePos a
