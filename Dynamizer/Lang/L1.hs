module Dynamizer.Lang.L1( TypeWithLoc
         , ExpF1
         , L1) where

import           Text.Parsec.Pos (SourcePos)

import           Dynamizer.Lang.Syntax

type WithLoc a = Ann SourcePos a

type TypeWithLoc = WithLoc Type
type ExpF1 = ExpF TypeWithLoc
type L1 = WithLoc ExpF1
