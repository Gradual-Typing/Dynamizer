{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}


module Annotizer where

import Control.Arrow((***))
import Control.Monad.Extra(concatMapM)
import Control.Monad.State.Lazy
import qualified Data.Bifunctor as B
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import L1

localLattice :: L1 -> L2
localLattice = mapExp $ B.first (\t->(lattice t,t))

fixType :: L2 -> L1
fixType = mapExp $ B.first snd

pick :: L2 -> [Int] -> L1
pick e [] = fixType e
pick (Ann s' e) nl = Ann s' $ fst $ pickExpF nl e
  where
    pickExpFTraverse :: [Int] -> [L2] -> ([L1], [Int])
    pickExpFTraverse ns [] = ([],ns)
    pickExpFTraverse ns (Ann s p:ps) = let (p',ns') = pickExpF ns p
                                           (ps',ns'') = pickExpFTraverse ns' ps
                                       in (Ann s p':ps',ns'')
      
    pickExpF :: [Int] -> ExpF2 L2 -> (ExpF1 L1, [Int])
    pickExpF ns (Op f es) =
      let (es',ns') = pickExpFTraverse ns es
      in (Op f es',ns')
    pickExpF ns (If (Ann s1 e1) (Ann s2 e2) (Ann s3 e3)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
          (e3',ns3) = pickExpF ns2 e3
      in (If (Ann s1 e1') (Ann s2 e2') (Ann s3 e3'),ns3)
    pickExpF ns (TopLevel e1 e2) =
      let (e1',ns1) = pickExpFTraverse ns e1
          (e2',ns2) = pickExpFTraverse ns1 e2
      in (TopLevel e1' e2',ns2)
    pickExpF ns (App (Ann s e1) es) =
      let (e1',ns1) = pickExpF ns e1
          (es',ns') = pickExpFTraverse ns1 es
      in (App (Ann s e1') es',ns')
    pickExpF (n:ns) (Lam x (Ann s e') (t,_)) =
      let (e'',ns') = pickExpF ns e'
      in (Lam x (Ann s e'') (t !! n),ns')
    pickExpF (n:ns) (Bind x (t,_) (Ann s e')) =
      let (e'',ns') = pickExpF ns e'
      in (Bind x (t !! n) (Ann s e''),ns')
    pickExpF ns (Ref (Ann s e')) =
      let (e'',ns') = pickExpF ns e'
      in (Ref (Ann s e''), ns')
    pickExpF ns (DeRef (Ann s e')) =
      let (e'',ns') = pickExpF ns e'
      in (DeRef (Ann s e''), ns')
    pickExpF ns (Assign (Ann s1 e1) (Ann s2 e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (Assign (Ann s1 e1') (Ann s2 e2'), ns2)
    pickExpF ns (GRef (Ann s e')) =
      let (e'',ns') = pickExpF ns e'
      in (GRef (Ann s e''), ns')
    pickExpF ns (GDeRef (Ann s e')) =
      let (e'',ns') = pickExpF ns e'
      in (GDeRef (Ann s e''), ns')
    pickExpF ns (GAssign (Ann s1 e1) (Ann s2 e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (GAssign (Ann s1 e1') (Ann s2 e2'), ns2)
    pickExpF ns (MRef (Ann s e')) =
      let (e'',ns') = pickExpF ns e'
      in (MRef (Ann s e''), ns')
    pickExpF ns (MDeRef (Ann s e')) =
      let (e'',ns') = pickExpF ns e'
      in (MDeRef (Ann s e''), ns')
    pickExpF ns (MAssign (Ann s1 e1) (Ann s2 e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (MAssign (Ann s1 e1') (Ann s2 e2'), ns2)
    pickExpF ns (Vect (Ann s1 e1) (Ann s2 e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (Vect (Ann s1 e1') (Ann s2 e2'), ns2)
    pickExpF ns (VectRef (Ann s1 e1) (Ann s2 e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (VectRef (Ann s1 e1') (Ann s2 e2'), ns2)
    pickExpF ns (VectSet (Ann s1 e1) (Ann s2 e2) (Ann s3 e3)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
          (e3',ns3) = pickExpF ns2 e3
      in (VectSet (Ann s1 e1') (Ann s2 e2') (Ann s3 e3'), ns3)
    pickExpF ns (GVect (Ann s1 e1) (Ann s2 e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (GVect (Ann s1 e1') (Ann s2 e2'), ns2)
    pickExpF ns (GVectRef (Ann s1 e1) (Ann s2 e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (GVectRef (Ann s1 e1') (Ann s2 e2'), ns2)
    pickExpF ns (GVectSet (Ann s1 e1) (Ann s2 e2) (Ann s3 e3)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
          (e3',ns3) = pickExpF ns2 e3
      in (GVectSet (Ann s1 e1') (Ann s2 e2') (Ann s3 e3'), ns3)
    pickExpF ns (MVect (Ann s1 e1) (Ann s2 e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (MVect (Ann s1 e1') (Ann s2 e2'), ns2)
    pickExpF ns (MVectRef (Ann s1 e1) (Ann s2 e2)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
      in (MVectRef (Ann s1 e1') (Ann s2 e2'), ns2)
    pickExpF ns (MVectSet (Ann s1 e1) (Ann s2 e2) (Ann s3 e3)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
          (e3',ns3) = pickExpF ns2 e3
      in (MVectSet (Ann s1 e1') (Ann s2 e2') (Ann s3 e3'), ns3)
    pickExpF ns (Let e1 (Ann s e2)) =
      let (e1',ns') = pickExpFTraverse ns e1
          (e2',ns2) = pickExpF ns' e2
      in (Let e1' (Ann s e2'),ns2)
    pickExpF ns (Tuple es) =
      let (es',ns') = pickExpFTraverse ns es
      in (Tuple es',ns')
    pickExpF ns (TupleProj (Ann s e1) i) =
      let (e',ns') = pickExpF ns e1
      in (TupleProj (Ann s e') i, ns')
    pickExpF ns (Letrec e1 (Ann s e2)) =
      let (e1',ns') = pickExpFTraverse ns e1
          (e2',ns2) = pickExpF ns' e2
      in (Letrec e1' (Ann s e2'),ns2)
    pickExpF (n:ns) (As (Ann s e') (t,_)) =
      let (e'',ns') = pickExpF ns e'
      in (As (Ann s e'') (t !! n),ns')
    pickExpF ns (Begin e1 (Ann s e2)) =
      let (e1',ns') = pickExpFTraverse ns e1
          (e2',ns2) = pickExpF ns' e2
      in (Begin e1' (Ann s e2'),ns2)
    pickExpF (n:ns) (Repeat i a (Ann s1 e1) (Ann s2 e2) (Ann s3 e3) (Ann s4 b) (t,_)) =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
          (b',ns3) = pickExpF ns2 b
          (e3',ns4) = pickExpF ns3 e3
      in (Repeat i a (Ann s1 e1') (Ann s2 e2') (Ann s3 e3') (Ann s4 b') (t !! n), ns4)
    pickExpF ns (Time (Ann s e')) =
      let (e'',ns') = pickExpF ns e'
      in (Time (Ann s e''), ns')
    pickExpF (n:ns) (DConst x (t,_) (Ann s e')) =
      let (e'',ns') = pickExpF ns e'
      in (DConst x (t !! n) (Ann s e''),ns')
    pickExpF (n:ns) (DLam x xs (Ann s e') (t,_)) =
      let (e'',ns') = pickExpF ns e'
      in (DLam x xs (Ann s e'') (t !! n),ns')
    pickExpF ns (P p) = (P p,ns)
    pickExpF _ _ = error "internal error"

replaceTypes :: M.Map SourcePos Type -> L1 -> L1
replaceTypes src2pos = foldAnn (\x y -> Ann x (replaceTypes' src2pos x y))
  where
    dyn :: Type -> Type
    dyn (ArrTy l _) = ArrTy (replicate (length l) Dyn) Dyn
    dyn _           = Dyn

    replaceTypes' :: M.Map SourcePos Type -> SourcePos -> ExpF Type L1 -> ExpF Type L1
    replaceTypes' tb s (Lam args e t)      =
      Lam args e $ fromMaybe (dyn t) $ M.lookup s tb
    replaceTypes' tb s (Bind x t e)        =
      Bind x (fromMaybe (dyn t) $ M.lookup s tb) e
    replaceTypes' tb s (As e t)            =
      As e $ fromMaybe (dyn t) $ M.lookup s tb
    replaceTypes' tb s (DConst x t e)      =
      DConst x (fromMaybe (dyn t) $ M.lookup s tb) e
    replaceTypes' tb s (DLam x xs e t)     =
      DLam x xs e $ fromMaybe (dyn t) $ M.lookup s tb
    replaceTypes' tb s (Repeat ii a e1 e2 e b t)  =
      Repeat ii a e1 e2 e b $ fromMaybe (dyn t) $ M.lookup s tb
    replaceTypes' _ _ e                    = e

data TypeInfo = TypeInfo { typePos               :: SourcePos
                         , typeNodesCount        :: Int
                         , typeNodesCountLattice :: [Int]
                         , typeLattice           :: [Type]
                         } deriving Show

-- computes the sizes of local lattices
  -- max # of nodes, src pos, # nodes, ...
genTypeInfo :: SourcePos -> L1 -> State Int [TypeInfo]
genTypeInfo _ (Ann src expr)  = genTypeInfo' src expr
  where
    genTypeInfo' s (Lam _ e' t)        =
      get >>= \y -> let st = static t
                        ts = lattice t
                    in put (st+y) >> (:) <$> return (TypeInfo s st (map static ts) ts) <*> genTypeInfo s e'
    genTypeInfo' s (Bind _ t e) = do
      y <- get
      let st  = static t
          ts = lattice t
      put (st+y)
      (:) <$> return (TypeInfo s st (map static ts) ts) <*> genTypeInfo s e
    genTypeInfo' s (As e' t)           =
      get >>= \y ->
      let st = static t
          ts = lattice t
      in put (st+y) >> (:) <$> return (TypeInfo s st (map static ts) ts) <*> genTypeInfo s e'
    genTypeInfo' s (Repeat _ _ e1 e2 e3 b t) = do
      a1 <- genTypeInfo s e1
      a2 <- genTypeInfo s e2
      y <- get
      let st = static t
          ts = lattice t
      put (st+y)
      let a3 = [(TypeInfo s st (map static ts) ts)]
      a4 <- genTypeInfo s b
      a5 <- genTypeInfo s e3
      return (a1 ++ a2 ++ a3 ++ a4 ++ a5)
    genTypeInfo' s (DConst _ t e) = do
      y <- get
      let st  = static t
          ts = lattice t
      put (st+y)
      (:) <$> return (TypeInfo s st (map static ts) ts) <*> genTypeInfo s e
    genTypeInfo' s (DLam _ _ e t) = do
      y <- get
      let st  = static t
          ts = lattice t
      put (st+y)
      (:) <$> return (TypeInfo s st (map static ts) ts) <*> genTypeInfo s e
    genTypeInfo' s (Op _ es)           = concatMapM (genTypeInfo s) es
    genTypeInfo' s (If e1 e2 e3)       =
      (\x y z -> x++y++z)
      <$> genTypeInfo s e1
      <*> genTypeInfo s e2
      <*> genTypeInfo s e3
    genTypeInfo' s (App e1 es)         =
      (++) <$> genTypeInfo s e1
      <*> concatMapM (genTypeInfo s) es
    genTypeInfo' s (TopLevel d e)      =
      (++)
      <$> concatMapM (genTypeInfo s) d
      <*> concatMapM (genTypeInfo s) e
    genTypeInfo' s (Ref e')            = genTypeInfo s e'
    genTypeInfo' s (DeRef e')          = genTypeInfo s e'
    genTypeInfo' s (Assign e1 e2)      =
      (++)
      <$> genTypeInfo s e1
      <*> genTypeInfo s e2
    genTypeInfo' s (GRef e')           = genTypeInfo s e'
    genTypeInfo' s (GDeRef e')         = genTypeInfo s e'
    genTypeInfo' s (GAssign e1 e2)     =
      (++)
      <$> genTypeInfo s e1
      <*> genTypeInfo s e2
    genTypeInfo' s (MRef e')           = genTypeInfo s e'
    genTypeInfo' s (MDeRef e')         = genTypeInfo s e'
    genTypeInfo' s (MAssign e1 e2)     =
      (++)
      <$> genTypeInfo s e1
      <*> genTypeInfo s e2
    genTypeInfo' s (Vect e1 e2)        =
      (++)
      <$> genTypeInfo s e1
      <*> genTypeInfo s e2
    genTypeInfo' s (VectRef e1 e2)     =
      (++)
      <$> genTypeInfo s e1
      <*> genTypeInfo s e2
    genTypeInfo' s (VectSet e1 e2 e3)  =
      (\x y z -> x++y++z)
      <$> genTypeInfo s e1
      <*> genTypeInfo s e2
      <*> genTypeInfo s e3
    genTypeInfo' s (GVect e1 e2)       =
      (++)
      <$> genTypeInfo s e1
      <*> genTypeInfo s e2
    genTypeInfo' s (GVectRef e1 e2)    =
      (++)
      <$> genTypeInfo s e1
      <*> genTypeInfo s e2
    genTypeInfo' s (GVectSet e1 e2 e3) =
      (\x y z -> x++y++z)
      <$> genTypeInfo s e1
      <*> genTypeInfo s e2
      <*> genTypeInfo s e3
    genTypeInfo' s (MVect e1 e2)       =
      (++)
      <$> genTypeInfo s e1
      <*> genTypeInfo s e2
    genTypeInfo' s (MVectRef e1 e2)    =
      (++)
      <$> genTypeInfo s e1
      <*> genTypeInfo s e2
    genTypeInfo' s (MVectSet e1 e2 e3) =
      (\x y z -> x++y++z)
      <$> genTypeInfo s e1
      <*> genTypeInfo s e2
      <*> genTypeInfo s e3
    genTypeInfo' s (Tuple es)          = concatMapM (genTypeInfo s) es
    genTypeInfo' s (TupleProj e _)     = genTypeInfo s e
    genTypeInfo' s (Let e1 e2)         =
      (++)
      <$> concatMapM (genTypeInfo s) e1
      <*> genTypeInfo s e2
    genTypeInfo' s (Letrec e1 e2)      =
      (++)
      <$> concatMapM (genTypeInfo s) e1
      <*> genTypeInfo s e2
    genTypeInfo' s (Begin e1 e2)       =
      (++)
      <$> concatMapM (genTypeInfo s) e1
      <*> genTypeInfo s e2
    genTypeInfo' s (Time e')           = genTypeInfo s e'
    genTypeInfo' _ _ = return []

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

instance Gradual L1 where
  lattice (Ann i e)           = Ann i <$> lattice e
  count (Ann _ e)             = count e
  static (Ann _ e)            = static e

instance Gradual e => Gradual (ExpF1 e) where
  lattice (Lam args e t)      = Lam args <$> lattice e <*> lattice t
  lattice (Bind x t e)          = Bind x <$> lattice t <*> lattice e
  lattice (As e t)            = As <$> lattice e <*> lattice t
  lattice (TopLevel d e)      = TopLevel <$> mapM lattice d <*> mapM lattice e
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
  lattice (Tuple es)          = Tuple <$> mapM lattice es
  lattice (TupleProj e i)     = TupleProj <$> lattice e <*> return i
  lattice (Let e1 e2)         = Let <$> mapM lattice e1 <*> lattice e2
  lattice (Letrec e1 e2)      = Letrec <$> mapM lattice e1 <*> lattice e2
  lattice (Begin e' e)        = Begin <$> mapM lattice e' <*> lattice e
  lattice (Time e)            = Time <$> lattice e
  lattice (DConst x t e)      = DConst x <$> lattice t <*> lattice e
  lattice (DLam x xs e t)     = DLam x xs <$> lattice e <*> lattice t
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
  count (TopLevel d e)        = let c1 = map count d
                                    c  = map count e
                                in (product (map fst c1) * product (map fst c),
                                    sum (map snd c1) + sum (map snd c))
  count (Lam _ e t)           = let c1 = count e
                                    c2 = count t
                                in ((*) (fst c1) *** (+) (snd c1)) c2
  count (Bind _ t e)          =
    let c1 = count e
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
  count (Tuple es)            = let c = map count es
                                in (product (map fst c), sum (map snd c))
  count (TupleProj e _)       = count e
  count (Let e1 e2)           = let c1 = map count e1
                                    c2 = count e2
                                in ((*) (product (map fst c1)) *** (+) (sum (map snd c1))) c2
  count (Letrec e1 e2)        = let c1 = map count e1
                                    c2 = count e2
                                in ((*) (product (map fst c1)) *** (+) (sum (map snd c1))) c2
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
  count (DConst _ t e)        =
    let c1 = count e
        c2 = count t
    in ((*) (fst c1) *** (+) (snd c1)) c2
  count (DLam _ _ e t)        =
    let c1 = count e
        c2 = count t
    in ((*) (fst c1) *** (+) (snd c1)) c2
  count _                     = (1,0)

  static (Op _ es)            = sum (map static es)
  static (If e1 e2 e3)        = static e1 + static e2 + static e3
  static (App e1 es)          = static e1 + sum (map static es)
  static (TopLevel d e)       = sum (map static d) + sum (map static e)
  static (Lam _ e t)          = static t + static e
  static (Bind _ t e)         = static e + static t
  static (Ref e)              = static e
  static (DeRef e)            = static e
  static (Assign e1 e2)       = static e1 + static e2
  static (GRef e)             = static e
  static (GDeRef e)           = static e
  static (GAssign e1 e2)      = static e1 + static e2
  static (MRef e)             = static e
  static (MDeRef e)           = static e
  static (MAssign e1 e2)      = static e1 + static e2
  static (Vect e1 e2)         = static e1 + static e2
  static (VectRef e1 e2)      = static e1 + static e2
  static (VectSet e1 e2 e3)   = static e1 + static e2 + static e3
  static (GVect e1 e2)        = static e1 + static e2
  static (GVectRef e1 e2)     = static e1 + static e2
  static (GVectSet e1 e2 e3)  = static e1 + static e2 + static e3
  static (MVect e1 e2)        = static e1 + static e2
  static (MVectRef e1 e2)     = static e1 + static e2
  static (MVectSet e1 e2 e3)  = static e1 + static e2 + static e3
  static (Tuple es)           = sum $ map static es
  static (TupleProj e _)      = static e
  static (Let e1 e2)          = sum (map static e1) + static e2
  static (Letrec e1 e2)       = sum (map static e1) + static e2
  static (As e t)             = static t + static e
  static (Begin e' e)         = static e + sum (map static e')
  static (Repeat _ _ e1 e2 e3 b t) = static e1 + static e2 + static e3 + static b + static t
  static (Time e)             = static e
  static (DConst _ t e)       = static e + static t
  static (DLam _ _ e t)       = static e + static t
  static _                    = 0

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
  count (TupleTy ts)    = let c1 = map count ts
                          in (1 + product (map fst c1),
                              1 + sum (map snd c1))
  count Dyn             = (1,1)
  count _               = (2,1)

  static Dyn            = 0
  static (RefTy t)      = 1 + static t
  static (GRefTy t)     = 1 + static t
  static (MRefTy t)     = 1 + static t
  static (VectTy t)     = 1 + static t
  static (GVectTy t)    = 1 + static t
  static (MVectTy t)    = 1 + static t
  static (FunTy t1 t2)  = 1 + sum (map static (t2:t1))
  static (ArrTy t1 t2)  = sum (map static (t2:t1))
  static (TupleTy ts)   = 1 + sum (map static ts)
  static _              = 1
