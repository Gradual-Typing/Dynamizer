{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}


module Annotizer where

import Control.Arrow((***))
import Control.Monad.Extra(concatMapM)
import Data.Foldable(foldrM)
import Control.Monad.State.Lazy
import qualified Data.Bifunctor as B

import L1

localLattice :: L1 -> L2
localLattice = mapExp $ B.first (\t->(lattice t,t))

fixType :: L2 -> L1
fixType = mapExp $ B.first snd

pick :: L2 -> [Int] -> L1
pick e [] = fixType e
pick (Ann _ e) nl = Ann undefined $ fst $ pickExpF nl e
  where
    pickExpFTraverse :: [Int] -> [L2] -> ([L1], [Int])
    pickExpFTraverse ns [] = ([],ns)
    pickExpFTraverse ns (Ann _ p:ps) = let (p',ns') = pickExpF ns p
                                           (ps',ns'') = pickExpFTraverse ns' ps
                                       in (Ann undefined p':ps',ns'')
      
    pickExpF :: [Int] -> ExpF2 L2 -> (ExpF1 L1, [Int])
    pickExpF ns (Op f es) =
      let (es',ns') = pickExpFTraverse ns es
      in (Op f es',ns')
    pickExpF ns (If (Ann _ e1) (Ann _ e2) (Ann _ e3)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
          (e3',ns3) = pickExpF ns2 e3
      in (If (Ann undefined e1') (Ann undefined e2') (Ann undefined e3'),ns3)
    pickExpF ns (TopLevelDefs (Defs e1) e2) =
      let (e1',ns1) = pickExpFDefs ns e1
          (e2',ns2) = pickExpFTraverse ns1 e2
      in (TopLevelDefs (Defs e1') e2',ns2)
    pickExpF ns (App (Ann _ e1) es) =
      let (e1',ns1) = pickExpF ns e1
          (es',ns') = pickExpFTraverse ns1 es
      in (App (Ann undefined e1') es',ns')
    pickExpF (n:ns) (Lam x (Ann _ e') (t,_)) =
      let (e'',ns') = pickExpF ns e'
      in (Lam x (Ann undefined e'') (t !! n),ns')
    pickExpF ns (Ref (Ann _ e')) =
      let (e'',ns') = pickExpF ns e'
      in (Ref (Ann undefined e''), ns')
    pickExpF ns (DeRef (Ann _ e')) =
      let (e'',ns') = pickExpF ns e'
      in (DeRef (Ann undefined e''), ns')
    pickExpF ns (Assign (Ann _ e1) (Ann _ e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (Assign (Ann undefined e1') (Ann undefined e2'), ns2)
    pickExpF ns (GRef (Ann _ e')) =
      let (e'',ns') = pickExpF ns e'
      in (GRef (Ann undefined e''), ns')
    pickExpF ns (GDeRef (Ann _ e')) =
      let (e'',ns') = pickExpF ns e'
      in (GDeRef (Ann undefined e''), ns')
    pickExpF ns (GAssign (Ann _ e1) (Ann _ e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (GAssign (Ann undefined e1') (Ann undefined e2'), ns2)
    pickExpF ns (MRef (Ann _ e')) =
      let (e'',ns') = pickExpF ns e'
      in (MRef (Ann undefined e''), ns')
    pickExpF ns (MDeRef (Ann _ e')) =
      let (e'',ns') = pickExpF ns e'
      in (MDeRef (Ann undefined e''), ns')
    pickExpF ns (MAssign (Ann _ e1) (Ann _ e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (MAssign (Ann undefined e1') (Ann undefined e2'), ns2)
    pickExpF ns (Vect (Ann _ e1) (Ann _ e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (Vect (Ann undefined e1') (Ann undefined e2'), ns2)
    pickExpF ns (VectRef (Ann _ e1) (Ann _ e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (VectRef (Ann undefined e1') (Ann undefined e2'), ns2)
    pickExpF ns (VectSet (Ann _ e1) (Ann _ e2) (Ann _ e3)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
          (e3',ns3) = pickExpF ns2 e3
      in (VectSet (Ann undefined e1') (Ann undefined e2') (Ann undefined e3'), ns3)
    pickExpF ns (GVect (Ann _ e1) (Ann _ e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (GVect (Ann undefined e1') (Ann undefined e2'), ns2)
    pickExpF ns (GVectRef (Ann _ e1) (Ann _ e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (GVectRef (Ann undefined e1') (Ann undefined e2'), ns2)
    pickExpF ns (GVectSet (Ann _ e1) (Ann _ e2) (Ann _ e3)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
          (e3',ns3) = pickExpF ns2 e3
      in (GVectSet (Ann undefined e1') (Ann undefined e2') (Ann undefined e3'), ns3)
    pickExpF ns (MVect (Ann _ e1) (Ann _ e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (MVect (Ann undefined e1') (Ann undefined e2'), ns2)
    pickExpF ns (MVectRef (Ann _ e1) (Ann _ e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (MVectRef (Ann undefined e1') (Ann undefined e2'), ns2)
    pickExpF ns (MVectSet (Ann _ e1) (Ann _ e2) (Ann _ e3)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
          (e3',ns3) = pickExpF ns2 e3
      in (MVectSet (Ann undefined e1') (Ann undefined e2') (Ann undefined e3'), ns3)
    pickExpF ns (Let (Binds e1) (Ann _ e2)) =
      let (e1',ns') = pickExpFBinds ns e1
          (e2',ns2) = pickExpF ns' e2
      in (Let (Binds e1') (Ann undefined e2'),ns2)
    pickExpF ns (Letrec (Binds e1) (Ann _ e2)) =
      let (e1',ns') = pickExpFBinds ns e1
          (e2',ns2) = pickExpF ns' e2
      in (Letrec (Binds e1') (Ann undefined e2'),ns2)
    pickExpF (n:ns) (As (Ann _ e') (t,_)) =
      let (e'',ns') = pickExpF ns e'
      in (As (Ann undefined e'') (t !! n),ns')
    pickExpF ns (Begin e1 (Ann _ e2)) =
      let (e1',ns') = pickExpFTraverse ns e1
          (e2',ns2) = pickExpF ns' e2
      in (Begin e1' (Ann undefined e2'),ns2)
    pickExpF (n:ns) (Repeat i a (Ann _ e1) (Ann _ e2) (Ann _ e3) (Ann _ b) (t,_)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
          (b',ns3) = pickExpF ns2 b
          (e3',ns4) = pickExpF ns3 e3
      in (Repeat i a (Ann undefined e1') (Ann undefined e2') (Ann undefined e3') (Ann undefined b') (t !! n), ns4)
    pickExpF ns (Time (Ann _ e')) =
      let (e'',ns') = pickExpF ns e'
      in (Time (Ann undefined e''), ns')
    pickExpF ns (P p) = (P p,ns)
    pickExpF _ _ = error "internal error"

    pickExpFBind :: [Int] -> Bind ([Type],Type) L2 -> (Bind Type L1,[Int])
    pickExpFBind [] _ = error "The list of indices can not be empty while there are still typed bindings"
    pickExpFBind (n:ns) (Bind x (t,_) (Ann _ e')) =
      let (e'',ns') = pickExpF ns e'
      in (Bind x (t !! n) (Ann undefined e''),ns')

    pickExpFBinds :: [Int] -> [Bind ([Type],Type) L2] -> ([Bind Type L1], [Int])
    pickExpFBinds ns []     = ([],ns)
    pickExpFBinds ns (p:ps) = let (p',ns') = pickExpFBind ns p
                                  (ps',ns'') = pickExpFBinds ns' ps
                              in (p':ps',ns'')

    pickExpFDef :: [Int] -> Def ([Type],Type) L2 -> (Def Type L1,[Int])
    pickExpFDef [] _ = error "The list of indices can not be empty while there are still typed bindings"
    pickExpFDef (n:ns) (DConst x (t,_) (Ann _ e')) =
      let (e'',ns') = pickExpF ns e'
      in (DConst x (t !! n) (Ann undefined e''),ns')
    pickExpFDef (n:ns) (DLam x xs (Ann _ e') (t,_)) =
      let (e'',ns') = pickExpF ns e'
      in (DLam x xs (Ann undefined e'') (t !! n),ns')

    pickExpFDefs :: [Int] -> [Def ([Type],Type) L2] -> ([Def Type L1], [Int])
    pickExpFDefs ns [] = ([],ns)
    pickExpFDefs ns (p:ps) = let (p',ns') = pickExpFDef ns p
                                 (ps',ns'') = pickExpFDefs ns' ps
                             in (p':ps',ns'')

dyn :: Type -> Type
dyn (ArrTy l _) = ArrTy (replicate (length l) Dyn) Dyn
dyn _           = Dyn


class Gradual p where
  -- Generates the lattice of all possible gradually-typed versions.
  lattice :: p -> [p]
  -- Counts the number of less percise programs and the number of
  -- all type constructors
  count   :: p -> (Integer,Int)
  -- computes the percentage of dynamic code.
  dynamic :: Int -> p -> Double
  dynamic a e =
    if a > 0
    then fromIntegral (a - static e) / fromIntegral a
    else 0
  -- counts the number of static type nodes in a program
  static  :: p -> Int
  -- computes the sizes of local lattices and imposes an order on them
  -- order tracker,max # of nodes, index, # nodes, ...
  countTypeLattice :: p -> State (Int,Int) [(Int,Int,[Int],[Type])]
  replaceTypes :: p -> State [(Int,Type)] p

instance Gradual L1 where
  -- source information is not relevant
  lattice (Ann i e)           = Ann i <$> lattice e
  count (Ann _ e)             = count e
  static (Ann _ e)            = static e
  countTypeLattice (Ann _ e)  = countTypeLattice e
  replaceTypes (Ann i e)      = Ann i <$> replaceTypes e

instance Gradual e => Gradual (ExpF1 e) where
  lattice (Lam args e t)      = Lam args <$> lattice e <*> lattice t
  lattice (As e t)            = As <$> lattice e <*> lattice t
  lattice (TopLevelDefs d e)  = TopLevelDefs <$> lattice d <*> mapM lattice e
  lattice (Repeat i a e1 e2 e b t)  =
    Repeat i a <$> lattice e1 <*> lattice e2 <*> lattice e <*> lattice b <*> lattice t
  lattice (Op op es)          = Op op <$> mapM lattice es
  lattice (If e1 e2 e3)       = If <$> lattice e1 <*> lattice e2 <*> lattice e3
  lattice (App e1 es)         = App <$> lattice e1 <*> mapM lattice es
  lattice (Ref e)             = Ref <$> lattice e
  lattice (DeRef e)           = DeRef <$> lattice e
  lattice (Assign e1 e2)      = Assign <$> lattice e1 <*> lattice e2
  lattice (GRef e)            = GRef <$> lattice e
  lattice (GDeRef e)          = GDeRef <$> lattice e
  lattice (GAssign e1 e2)     = GAssign <$> lattice e1 <*> lattice e2
  lattice (MRef e)            = MRef <$> lattice e
  lattice (MDeRef e)          = MDeRef <$> lattice e
  lattice (MAssign e1 e2)     = MAssign <$> lattice e1 <*> lattice e2
  lattice (Vect e1 e2)        = Vect <$> lattice e1 <*> lattice e2
  lattice (VectRef e1 e2)     = VectRef <$> lattice e1 <*> lattice e2
  lattice (VectSet e1 e2 e3)  = VectSet <$> lattice e1 <*> lattice e2
                                <*> lattice e3
  lattice (GVect e1 e2)       = GVect <$> lattice e1 <*> lattice e2
  lattice (GVectRef e1 e2)    = GVectRef <$> lattice e1 <*> lattice e2
  lattice (GVectSet e1 e2 e3) = GVectSet <$> lattice e1 <*> lattice e2
                                <*> lattice e3
  lattice (MVect e1 e2)       = MVect <$> lattice e1 <*> lattice e2
  lattice (MVectRef e1 e2)    = MVectRef <$> lattice e1 <*> lattice e2
  lattice (MVectSet e1 e2 e3) = MVectSet <$> lattice e1 <*> lattice e2
                                <*> lattice e3
  lattice (Let e1 e2)         = Let <$> lattice e1 <*> lattice e2
  lattice (Letrec e1 e2)      = Letrec <$> lattice e1 <*> lattice e2
  lattice (Begin e' e)        = Begin <$> mapM lattice e' <*> lattice e
  lattice (Time e)            = Time <$> lattice e
  lattice e                   = return e

  count (Op _ es)             = let c = map count es
                                in (product $ map fst c, sum $ map snd c)
  count (If e1 e2 e3)         = let c1 = count e1
                                    c2 = count e2
                                    c3 = count e3
                                in  ((*) (fst c1 * fst c2) *** (+) (snd c1 + snd c2)) c3
  count (App e1 es)           = let c1 = count e1
                                    c = map count es
                                in (fst c1 * product (map fst c),
                                    snd c1 + sum (map snd c))
  count (TopLevelDefs d e)    = let c1 = count d
                                    c  = map count e
                                in (fst c1 * product (map fst c),
                                    snd c1 + sum (map snd c))
  count (Lam _ e t)           = let c1 = count e
                                    c2 = count t
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (Ref e)               = count e
  count (DeRef e)             = count e
  count (Assign e1 e2)        = let c1 = count e1
                                    c2 = count e2
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (GRef e)              = count e
  count (GDeRef e)            = count e
  count (GAssign e1 e2)       = let c1 = count e1
                                    c2 = count e2
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (MRef e)              = count e
  count (MDeRef e)            = count e
  count (MAssign e1 e2)       = let c1 = count e1
                                    c2 = count e2
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (Vect e1 e2)          = let c1 = count e1
                                    c2 = count e2
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (VectRef e1 e2)       = let c1 = count e1
                                    c2 = count e2
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (VectSet e1 e2 e3)    = let c1 = count e1
                                    c2 = count e2
                                    c3 = count e3
                                in ((*) (fst c1 * fst c2) *** (+) (snd c1 + snd c2)) c3
  count (GVect e1 e2)         = let c1 = count e1
                                    c2 = count e2
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (GVectRef e1 e2)      = let c1 = count e1
                                    c2 = count e2
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (GVectSet e1 e2 e3)   = let c1 = count e1
                                    c2 = count e2
                                    c3 = count e3
                                in ((*) (fst c1 * fst c2) *** (+) (snd c1 + snd c2)) c3
  count (MVect e1 e2)         = let c1 = count e1
                                    c2 = count e2
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (MVectRef e1 e2)      = let c1 = count e1
                                    c2 = count e2
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (MVectSet e1 e2 e3)   = let c1 = count e1
                                    c2 = count e2
                                    c3 = count e3
                                in ((*) (fst c1 * fst c2) *** (+) (snd c1 + snd c2)) c3
  count (Let e1 e2)           = let c1 = count e1
                                    c2 = count e2
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (Letrec e1 e2)        = let c1 = count e1
                                    c2 = count e2
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (As e t)              = let c1 = count e
                                    c2 = count t
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (Begin e' e)          = let c1 = map count e'
                                    c2 = count e
                                in ((*) (product (map fst c1)) *** (+) (sum (map snd c1))) c2
  count (Repeat _ _ e1 e2 e3 b t)   = let c1 = count e1
                                          c2 = count e2
                                          c3 = count e3
                                          c4 = count b
                                          c5 = count t
                                      in ((*) (fst c1 * fst c2 * fst c3 * fst c4) *** (+) (snd c1 + snd c2 + snd c3 + snd c4)) c5
  count (Time e)              = count e
  count _                     = (1,0)

  static (Op _ es)           = sum (map static es)
  static (If e1 e2 e3)       = static e1 + static e2 + static e3
  static (App e1 es)         = static e1 + sum (map static es)
  static (TopLevelDefs d e)  = static d + sum (map static e)
  static (Lam _ e t)         = static t + static e
  static (Ref e)             = static e
  static (DeRef e)           = static e
  static (Assign e1 e2)      = static e1 + static e2
  static (GRef e)            = static e
  static (GDeRef e)          = static e
  static (GAssign e1 e2)     = static e1 + static e2
  static (MRef e)            = static e
  static (MDeRef e)          = static e
  static (MAssign e1 e2)     = static e1 + static e2
  static (Vect e1 e2)        = static e1 + static e2
  static (VectRef e1 e2)     = static e1 + static e2
  static (VectSet e1 e2 e3)  = static e1 + static e2 + static e3
  static (GVect e1 e2)       = static e1 + static e2
  static (GVectRef e1 e2)    = static e1 + static e2
  static (GVectSet e1 e2 e3) = static e1 + static e2 + static e3
  static (MVect e1 e2)       = static e1 + static e2
  static (MVectRef e1 e2)    = static e1 + static e2
  static (MVectSet e1 e2 e3) = static e1 + static e2 + static e3
  static (Let e1 e2)         = static e1 + static e2
  static (Letrec e1 e2)      = static e1 + static e2
  static (As e t)            = static t + static e
  static (Begin e' e)        = static e + sum (map static e')
  static (Repeat _ _ e1 e2 e3 b t) = static e1 + static e2 + static e3 + static b + static t
  static (Time e)            = static e
  static _                   = 0

  -- order tracker,max # of nodes, index, # nodes, ...
  countTypeLattice (Op _ es)           = concatMapM countTypeLattice es
  countTypeLattice (If e1 e2 e3)       = (\x y z -> x++y++z) <$> countTypeLattice e1 <*> countTypeLattice e2 <*> countTypeLattice e3
  countTypeLattice (App e1 es)         = (++) <$> countTypeLattice e1 <*> concatMapM countTypeLattice es
  countTypeLattice (TopLevelDefs d e)  = (++) <$> countTypeLattice d <*> concatMapM countTypeLattice e
  countTypeLattice (Lam _ e' t)        =
    get >>= \(x,y) -> let s = static t
                          ts = lattice t
                      in put (x+1,s+y) >> (:) <$> return (x,s, map static ts,ts) <*> countTypeLattice e'
  countTypeLattice (Ref e')            = countTypeLattice e'
  countTypeLattice (DeRef e')          = countTypeLattice e'
  countTypeLattice (Assign e1 e2)      = (++) <$> countTypeLattice e1 <*> countTypeLattice e2
  countTypeLattice (GRef e')           = countTypeLattice e'
  countTypeLattice (GDeRef e')         = countTypeLattice e'
  countTypeLattice (GAssign e1 e2)     = (++) <$> countTypeLattice e1 <*> countTypeLattice e2
  countTypeLattice (MRef e')           = countTypeLattice e'
  countTypeLattice (MDeRef e')         = countTypeLattice e'
  countTypeLattice (MAssign e1 e2)     = (++) <$> countTypeLattice e1 <*> countTypeLattice e2
  countTypeLattice (Vect e1 e2)        = (++) <$> countTypeLattice e1 <*> countTypeLattice e2
  countTypeLattice (VectRef e1 e2)     = (++) <$> countTypeLattice e1 <*> countTypeLattice e2
  countTypeLattice (VectSet e1 e2 e3)  = (\x y z -> x++y++z) <$> countTypeLattice e1 <*> countTypeLattice e2 <*> countTypeLattice e3
  countTypeLattice (GVect e1 e2)       = (++) <$> countTypeLattice e1 <*> countTypeLattice e2
  countTypeLattice (GVectRef e1 e2)    = (++) <$> countTypeLattice e1 <*> countTypeLattice e2
  countTypeLattice (GVectSet e1 e2 e3) = (\x y z -> x++y++z) <$> countTypeLattice e1 <*> countTypeLattice e2 <*> countTypeLattice e3
  countTypeLattice (MVect e1 e2)       = (++) <$> countTypeLattice e1 <*> countTypeLattice e2
  countTypeLattice (MVectRef e1 e2)    = (++) <$> countTypeLattice e1 <*> countTypeLattice e2
  countTypeLattice (MVectSet e1 e2 e3) = (\x y z -> x++y++z) <$> countTypeLattice e1 <*> countTypeLattice e2 <*> countTypeLattice e3
  countTypeLattice (Let e1 e2)         = (++) <$> countTypeLattice e1 <*> countTypeLattice e2
  countTypeLattice (Letrec e1 e2)      = (++) <$> countTypeLattice e1 <*> countTypeLattice e2
  countTypeLattice (As e' t)           =
    get >>= \(x,y) -> let s = static t
                          ts = lattice t
                      in put (x+1,s+y) >> (:) <$> return (x,s, map static ts,ts) <*> countTypeLattice e'
  countTypeLattice (Begin e1 e2)       = (++) <$> concatMapM countTypeLattice e1 <*> countTypeLattice e2
  countTypeLattice (Repeat _ _ e1 e2 e3 b t) = do
    a1 <- countTypeLattice e1
    a2 <- countTypeLattice e2
    (x,y) <- get
    let s = static t
        ts = lattice t
    put (x+1,s+y)
    let a3 = [(x,s, map static ts,ts)]
    a4 <- countTypeLattice b
    a5 <- countTypeLattice e3
    return (a1 ++ a2 ++ a3 ++ a4 ++ a5)
  countTypeLattice (Time e')           = countTypeLattice e'
  countTypeLattice _ = return []

  replaceTypes (Op op es)          = Op op <$> mapM replaceTypes es
  replaceTypes (If e1 e2 e3)       = If <$> replaceTypes e1 <*> replaceTypes e2 <*> replaceTypes e3
  replaceTypes (App e1 es)         = App <$> replaceTypes e1 <*> mapM replaceTypes es
  replaceTypes (TopLevelDefs d e)  = TopLevelDefs <$> replaceTypes d <*> mapM replaceTypes e
  replaceTypes (Lam args e t)      = do
    ll <- get
    case ll of
      ts@((i,t'):l) ->
        if i == 0
        then do put (map (\(x,y)->(x-1,y)) l)
                Lam args <$> replaceTypes e <*> return t'
        else do put (map (\(x,y)->(x-1,y)) ts)
                Lam args <$> replaceTypes e <*> return (dyn t)
      _ -> return (Lam args e $ dyn t)
  replaceTypes (Ref e)             = Ref <$> replaceTypes e
  replaceTypes (DeRef e)           = DeRef <$> replaceTypes e
  replaceTypes (Assign e1 e2)      = Assign <$> replaceTypes e1 <*> replaceTypes e2
  replaceTypes (GRef e)            = GRef <$> replaceTypes e
  replaceTypes (GDeRef e)          = GDeRef <$> replaceTypes e
  replaceTypes (GAssign e1 e2)     = GAssign <$> replaceTypes e1 <*> replaceTypes e2
  replaceTypes (MRef e)            = MRef <$> replaceTypes e
  replaceTypes (MDeRef e)          = MDeRef <$> replaceTypes e
  replaceTypes (MAssign e1 e2)     = MAssign <$> replaceTypes e1 <*> replaceTypes e2
  replaceTypes (Vect e1 e2)        = Vect <$> replaceTypes e1 <*> replaceTypes e2
  replaceTypes (VectRef e1 e2)     = VectRef <$> replaceTypes e1 <*> replaceTypes e2
  replaceTypes (VectSet e1 e2 e3)  = VectSet <$> replaceTypes e1 <*> replaceTypes e2 <*> replaceTypes e3
  replaceTypes (GVect e1 e2)       = GVect <$> replaceTypes e1 <*> replaceTypes e2
  replaceTypes (GVectRef e1 e2)    = GVectRef <$> replaceTypes e1 <*> replaceTypes e2
  replaceTypes (GVectSet e1 e2 e3) = GVectSet <$> replaceTypes e1 <*> replaceTypes e2 <*> replaceTypes e3
  replaceTypes (MVect e1 e2)       = MVect <$> replaceTypes e1 <*> replaceTypes e2
  replaceTypes (MVectRef e1 e2)    = MVectRef <$> replaceTypes e1 <*> replaceTypes e2
  replaceTypes (MVectSet e1 e2 e3) = MVectSet <$> replaceTypes e1 <*> replaceTypes e2 <*> replaceTypes e3
  replaceTypes (Let e1 e2)         = Let <$> replaceTypes e1 <*> replaceTypes e2
  replaceTypes (Letrec e1 e2)      = Letrec <$> replaceTypes e1 <*> replaceTypes e2
  replaceTypes (As e t)            = do
    ll <- get
    case ll of
      ts@((i,t'):l) ->
        if i == 0
        then do put (map (\(x,y)->(x-1,y)) l)
                As <$> replaceTypes e <*> return t'
        else do put (map (\(x,y)->(x-1,y)) ts)
                As <$> replaceTypes e <*> return (dyn t)
      _ -> return (As e $ dyn t)
  replaceTypes (Begin e' e)        = Begin <$> mapM replaceTypes e' <*> replaceTypes e
  replaceTypes (Repeat ii a e1 e2 e b t)  = do
    ll <- get
    case ll of
      ts@((i,t'):l) ->
        if i == 0
        then do put (map (\(x,y)->(x-1,y)) l)
                Repeat ii a <$> replaceTypes e1 <*> replaceTypes e2 <*> replaceTypes e <*> replaceTypes b <*> return t'
        else do put (map (\(x,y)->(x-1,y)) ts)
                Repeat ii a <$> replaceTypes e1 <*> replaceTypes e2 <*> replaceTypes e <*> replaceTypes b <*> return (dyn t)
      _ -> return (Repeat ii a e1 e2 e b $ dyn t)
  replaceTypes (Time e)            = Time <$> replaceTypes e
  replaceTypes e                   = return e
                                   
instance Gradual e => Gradual (Bind Type e) where
  lattice (Bind x t e)          = Bind x <$> lattice t <*> lattice e
  count (Bind _ t e)            =
    let c1 = count e
        c2 = count t
    in ((*) (fst c1) *** (+) (snd c1)) c2
  static (Bind _ t e)           = static e + static t
  countTypeLattice (Bind _ t e) = do
    (x,y) <- get
    let s  = static t
        ts = lattice t
    put (x+1,s+y)
    (:) <$> return (x,s, map static ts,ts) <*> countTypeLattice e
  replaceTypes (Bind xx t e)    = do
    ll <- get
    case ll of
      ts@((i,t'):l) ->
        if i == 0
        then do put (map (\(x,y)->(x-1,y)) l)
                Bind xx <$> return t' <*> replaceTypes e
        else do put (map (\(x,y)->(x-1,y)) ts)
                Bind xx <$> return (dyn t) <*> replaceTypes e
      _ -> return (Bind xx (dyn t) e)

instance Gradual e => Gradual (Binds Type e) where
  lattice (Binds l)          = Binds <$> mapM lattice l
  count (Binds l)            = let c = map count l
                               in (product (map fst c),
                                   sum (map snd c))
  static (Binds l)           = sum $ map static l
  countTypeLattice (Binds l) =
    foldrM (\a b -> (++) <$> countTypeLattice a <*> return b) [] l
  replaceTypes (Binds l)     = Binds <$> mapM replaceTypes l


instance Gradual e => Gradual (Def Type e) where
  lattice (DConst x t e)          = DConst x <$> lattice t <*> lattice e
  lattice (DLam x xs e t)         = DLam x xs <$> lattice e <*> lattice t
  count (DConst _ t e)            =
    let c1 = count e
        c2 = count t
    in ((*) (fst c1) *** (+) (snd c1)) c2
  count (DLam _ _ e t)            =
    let c1 = count e
        c2 = count t
    in ((*) (fst c1) *** (+) (snd c1)) c2
  static (DConst _ t e)           = static e + static t
  static (DLam _ _ t e)           = static e + static t
  countTypeLattice (DConst _ t e) = do
    (x,y) <- get
    let s  = static t
        ts = lattice t
    put (x+1,s+y)
    (:) <$> return (x,s, map static ts,ts) <*> countTypeLattice e
  countTypeLattice (DLam _ _ e t) = do
    (x,y) <- get
    let s  = static t
        ts = lattice t
    put (x+1,s+y)
    (:) <$> return (x,s, map static ts,ts) <*> countTypeLattice e
  replaceTypes (DConst xx t e)    = do
    ll <- get
    case ll of
      ts@((i,t'):l) ->
        if i == 0
        then do put (map (\(x,y)->(x-1,y)) l)
                DConst xx <$> return t' <*> replaceTypes e
        else do put (map (\(x,y)->(x-1,y)) ts)
                DConst xx <$> return (dyn t) <*> replaceTypes e
      _ -> return (DConst xx (dyn t) e)
  replaceTypes (DLam xx xs e t)    = do
    ll <- get
    case ll of
      ts@((i,t'):l) ->
        if i == 0
        then do put (map (\(x,y)->(x-1,y)) l)
                DLam xx xs <$> replaceTypes e <*> return t'
        else do put (map (\(x,y)->(x-1,y)) ts)
                DLam xx xs <$> replaceTypes e <*> return (dyn t)
      _ -> return (DLam xx xs e (dyn t))

instance Gradual e => Gradual (Defs Type e) where
  lattice (Defs l)          = Defs <$> mapM lattice l
  count (Defs l)            = let c = map count l
                               in (product (map fst c),
                                   sum (map snd c))
  static (Defs l)           = sum $ map static l
  countTypeLattice (Defs l) =
    foldrM (\a b -> (++) <$> countTypeLattice a <*> return b) [] l
  replaceTypes (Defs l)     = Defs <$> mapM replaceTypes l

instance Gradual Type where
  lattice (RefTy t)     = Dyn:(RefTy <$> lattice t)
  lattice (GRefTy t)    = Dyn:(GRefTy <$> lattice t)
  lattice (MRefTy t)    = Dyn:(MRefTy <$> lattice t)
  lattice (VectTy t)    = Dyn:(VectTy <$> lattice t)
  lattice (GVectTy t)   = Dyn:(GVectTy <$> lattice t)
  lattice (MVectTy t)   = Dyn:(MVectTy <$> lattice t)
  lattice (FunTy t1 t2) = Dyn:(FunTy <$> mapM lattice t1 <*> lattice t2)
  lattice (ArrTy t1 t2) = ArrTy <$> mapM lattice t1 <*> lattice t2
  lattice (TupleTy ts)  = Dyn:(TupleTy <$> mapM lattice ts)
  lattice Dyn           = [Dyn]
  lattice t             = [Dyn,t]

  count (RefTy t)       = let c = count t in (fst c + 1, snd c + 1)
  count (GRefTy t)      = let c = count t in (fst c + 1, snd c + 1)
  count (MRefTy t)      = let c = count t in (fst c + 1, snd c + 1)
  count (VectTy t)      = let c = count t in (fst c + 1, snd c + 1)
  count (GVectTy t)     = let c = count t in (fst c + 1, snd c + 1)
  count (MVectTy t)     = let c = count t in (fst c + 1, snd c + 1)
  count (FunTy t1 t2)   = let c1 = map count t1
                              c2 = count t2
                          in (1 + fst c2 * product (map fst c1),
                              1 + snd c2 + sum (map snd c1))
  count (ArrTy t1 t2)   = let c1 = map count t1
                              c2 = count t2
                          in (fst c2 * product (map fst c1),
                              snd c2 + sum (map snd c1))
  count Dyn             = (1,1)
  count _               = (2,1)

  static Dyn           = 0
  static (RefTy t)     = 1 + static t
  static (GRefTy t)    = 1 + static t
  static (MRefTy t)    = 1 + static t
  static (VectTy t)    = 1 + static t
  static (GVectTy t)   = 1 + static t
  static (MVectTy t)   = 1 + static t
  static (FunTy t1 t2) = 1 + sum (map static (t2:t1))
  static (ArrTy t1 t2) = sum (map static (t2:t1))
  static _             = 1

  countTypeLattice = error "countTypeLattice undefined over types"
  replaceTypes     = error "replaceTypes undefined over types"
