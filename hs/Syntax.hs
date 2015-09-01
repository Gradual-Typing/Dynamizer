module Syntax where

import Text.Parsec.Pos (SourcePos)

data Operator = Plus | Minus | Mult | Div | Eq | Ge | Gt | Le | Lt
              | ShiftR | ShiftL | BAnd | BOr
                deriving (Show,Eq,Read)

type Name = String
type Arg = (Name,Type)
type Args = [Arg]
type Bind = (Name,Type,L1)
type Binds = [Bind]

type Exp a = (SourcePos, a)

type L1 = Exp Exp1

data Exp1 =
  N1 Integer
  | B1 Bool
  | Unit
  | Op1 Operator [L1]
  | If1 L1 L1 L1
  | Var1 Name
  | App1 L1 [L1]
  | Lam1 Args L1 Type
  | GRef1 L1
  | GDeRef1 L1
  | GAssign1 L1 L1
  | MRef1 L1
  | MDeRef1 L1
  | MAssign1 L1 L1
  | GVect1 L1 L1 -- length value
  | GVectRef1 L1 L1 -- vect pos
  | GVectSet1 L1 L1 L1 -- vect pos value
  | MVect1 L1 L1
  | MVectRef1 L1 L1
  | MVectSet1 L1 L1 L1
  | Let1 Binds L1
  | Letrec1 Binds L1
  | As1 L1 Type
  | Begin1 [L1] L1
  | Repeat1 Name L1 L1 L1 -- int int e
  | TimerStart1 | TimerStop1 | TimerReport1
  deriving (Show,Eq)

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
  deriving (Show,Eq,Read)

type EnvT = [(Name, Type)]
