{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}


module Annotizer where

import Control.Arrow((***))

import L1

localLattice :: L1 -> L2
localLattice (Ann _ e) = Ann undefined $ localLatticeExpF e
  where
    localLatticeExpF :: ExpF1 L1 -> ExpF2 L2
    localLatticeExpF (Op op es)          = Op op $ map localLattice es
    localLatticeExpF (If e1 e2 e3)       = If (localLattice e1) (localLattice e2) $ localLattice e3
    localLatticeExpF (App e1 es)         = App (localLattice e1) $ map localLattice es
    localLatticeExpF (Lam x e' t)        = Lam x (localLattice e') $ (lattice t,t)
    localLatticeExpF (GRef e')           = GRef $ localLattice e'
    localLatticeExpF (GDeRef e')         = GDeRef $ localLattice e'
    localLatticeExpF (GAssign e1 e2)     = GAssign (localLattice e1) $ localLattice e2
    localLatticeExpF (MRef e')           = MRef $ localLattice e'
    localLatticeExpF (MDeRef e')         = MDeRef $ localLattice e'
    localLatticeExpF (MAssign e1 e2)     = MAssign (localLattice e1) $ localLattice e2
    localLatticeExpF (GVect e1 e2)       = GVect (localLattice e1) $ localLattice e2
    localLatticeExpF (GVectRef e1 e2)    = GVectRef (localLattice e1) $ localLattice e2
    localLatticeExpF (GVectSet e1 e2 e3) = GVectSet (localLattice e1) (localLattice e2) $ localLattice e3
    localLatticeExpF (MVect e1 e2)       = MVect (localLattice e1) $ localLattice e2
    localLatticeExpF (MVectRef e1 e2)    = MVectRef (localLattice e1) $ localLattice e2
    localLatticeExpF (MVectSet e1 e2 e3) = MVectSet (localLattice e1) (localLattice e2) $ localLattice e3
    localLatticeExpF (Let e1 e2)         = Let (map localLatticeBind e1) $ localLattice e2
    localLatticeExpF (Letrec e1 e2)      = Letrec (map localLatticeBind e1) $ localLattice e2
    localLatticeExpF (As e' t)           = As (localLattice e') $ (lattice t,t)
    localLatticeExpF (Begin e1 e2)       = Begin (map localLattice e1) $ localLattice e2
    localLatticeExpF (Repeat x e1 e2 e3) = Repeat x (localLattice e1) (localLattice e2) $ localLattice e3
    localLatticeExpF (P p) = P p

    localLatticeBind :: Bind L1 Type -> Bind L2 ([Type],Type)
    localLatticeBind (x,t,e') = (x,(lattice t,t),localLattice e')

fixType :: L2 -> L1
fixType (Ann _ e) = Ann undefined $ fixTypeExpF e
  where
    fixTypeExpF :: ExpF2 L2 -> ExpF1 L1
    fixTypeExpF (Op op es)          = Op op $ map fixType es
    fixTypeExpF (If e1 e2 e3)       = If (fixType e1) (fixType e2) $ fixType e3
    fixTypeExpF (App e1 es)         = App (fixType e1) $ map fixType es
    fixTypeExpF (Lam x e' (_,t))        = Lam x (fixType e') t
    fixTypeExpF (GRef e')           = GRef $ fixType e'
    fixTypeExpF (GDeRef e')         = GDeRef $ fixType e'
    fixTypeExpF (GAssign e1 e2)     = GAssign (fixType e1) $ fixType e2
    fixTypeExpF (MRef e')           = MRef $ fixType e'
    fixTypeExpF (MDeRef e')         = MDeRef $ fixType e'
    fixTypeExpF (MAssign e1 e2)     = MAssign (fixType e1) $ fixType e2
    fixTypeExpF (GVect e1 e2)       = GVect (fixType e1) $ fixType e2
    fixTypeExpF (GVectRef e1 e2)    = GVectRef (fixType e1) $ fixType e2
    fixTypeExpF (GVectSet e1 e2 e3) = GVectSet (fixType e1) (fixType e2) $ fixType e3
    fixTypeExpF (MVect e1 e2)       = MVect (fixType e1) $ fixType e2
    fixTypeExpF (MVectRef e1 e2)    = MVectRef (fixType e1) $ fixType e2
    fixTypeExpF (MVectSet e1 e2 e3) = MVectSet (fixType e1) (fixType e2) $ fixType e3
    fixTypeExpF (Let e1 e2)         = Let (map fixTypeBind e1) $ fixType e2
    fixTypeExpF (Letrec e1 e2)      = Letrec (map fixTypeBind e1) $ fixType e2
    fixTypeExpF (As e' (_,t))           = As (fixType e') t
    fixTypeExpF (Begin e1 e2)       = Begin (map fixType e1) $ fixType e2
    fixTypeExpF (Repeat x e1 e2 e3) = Repeat x (fixType e1) (fixType e2) $ fixType e3
    fixTypeExpF (P p) = P p

    fixTypeBind :: Bind L2 ([Type],Type) -> Bind L1 Type
    fixTypeBind (x,(_,t),e') = (x,t,fixType e')

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
    pickExpF ns (App (Ann _ e1) es) =
      let (e1',ns1) = pickExpF ns e1
          (es',ns') = pickExpFTraverse ns1 es
      in (App (Ann undefined e1') es',ns')
    pickExpF (n:ns) (Lam x (Ann _ e') (t,_)) =
      let (e'',ns') = pickExpF ns e'
      in (Lam x (Ann undefined e'') (t !! n),ns')
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
    pickExpF ns (Let e1 (Ann _ e2)) =
      let (e1',ns') = pickExpFBinds ns e1
          (e2',ns2) = pickExpF ns' e2
      in (Let e1' (Ann undefined e2'),ns2)
    pickExpF ns (Letrec e1 (Ann _ e2)) =
      let (e1',ns') = pickExpFBinds ns e1
          (e2',ns2) = pickExpF ns' e2
      in (Letrec e1' (Ann undefined e2'),ns2)
    pickExpF (n:ns) (As (Ann _ e') (t,_)) =
      let (e'',ns') = pickExpF ns e'
      in (As (Ann undefined e'') (t !! n),ns')
    pickExpF ns (Begin e1 (Ann _ e2)) =
      let (e1',ns') = pickExpFTraverse ns e1
          (e2',ns2) = pickExpF ns' e2
      in (Begin e1' (Ann undefined e2'),ns2)
    pickExpF ns (Repeat i (Ann _ e1) (Ann _ e2) (Ann _ e3))  =
      let (e1',ns1) = pickExpF ns e1
          (e2',ns2) = pickExpF ns1 e2
          (e3',ns3) = pickExpF ns2 e3
      in (Repeat i (Ann undefined e1') (Ann undefined e2') (Ann undefined e3'), ns3)
    pickExpF ns (P p) = (P p,ns)
    pickExpF _ _ = error "internal error"

    pickExpFBind :: [Int] -> Bind L2 ([Type],Type) -> (Bind L1 Type,[Int])
    pickExpFBind [] _ = error "The list of indices can not be empty while there are still typed bindings"
    pickExpFBind (n:ns) (x,(t,_),Ann _ e') =
      let (e'',ns') = pickExpF ns e'
      in ((x, t !! n, Ann undefined e''),ns')

    pickExpFBinds :: [Int] -> Binds L2 ([Type],Type) -> (Binds L1 Type, [Int])
    pickExpFBinds ns [] = ([],ns)
    pickExpFBinds ns (p:ps) = let (p',ns') = pickExpFBind ns p
                                  (ps',ns'') = pickExpFBinds ns' ps
                              in (p':ps',ns'')


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
  -- computes the number of type constructors.
  static  :: p -> Int
  countTypeLattice :: p -> [Int]

instance Gradual L1 where
  -- source information is not relevant
  lattice (Ann _ e)           = map (Ann undefined) $ lattice e
  count (Ann _ e)             = count e
  static (Ann _ e)            = static e
  countTypeLattice (Ann _ e)  = countTypeLattice e

instance Gradual e => Gradual (ExpF1 e) where
  lattice (Op op es)          = Op op <$> mapM lattice es
  lattice (If e1 e2 e3)       = If <$> lattice e1 <*> lattice e2 <*> lattice e3
  lattice (App e1 es)         = App <$> lattice e1 <*> mapM lattice es 
  lattice (Lam args e t)      = (Lam args) <$> lattice e <*> lattice t
  lattice (GRef e)            = GRef <$> lattice e
  lattice (GDeRef e)          = GDeRef <$> lattice e
  lattice (GAssign e1 e2)     = GAssign <$> lattice e1 <*> lattice e2
  lattice (MRef e)            = MRef <$> lattice e
  lattice (MDeRef e)          = MDeRef <$> lattice e
  lattice (MAssign e1 e2)     = MAssign <$> lattice e1 <*> lattice e2
  lattice (GVect e1 e2)       = GVect <$> lattice e1 <*> lattice e2
  lattice (GVectRef e1 e2)    = GVectRef <$> lattice e1 <*> lattice e2
  lattice (GVectSet e1 e2 e3) = GVectSet <$> lattice e1 <*> lattice e2
                                <*> lattice e3
  lattice (MVect e1 e2)       = MVect <$> lattice e1 <*> lattice e2
  lattice (MVectRef e1 e2)    = MVectRef <$> lattice e1 <*> lattice e2
  lattice (MVectSet e1 e2 e3) = MVectSet <$> lattice e1 <*> lattice e2
                                <*> lattice e3
  lattice (Let e1 e2)         = Let <$> mapM lattice e1 <*> lattice e2
  lattice (Letrec e1 e2)      = Letrec <$> mapM lattice e1 <*> lattice e2
  lattice (As e t)            = As <$> lattice e <*> lattice t
  lattice (Begin e' e)        = Begin <$> mapM lattice e' <*> lattice e
  lattice (Repeat i e1 e2 e)  = Repeat i <$> lattice e1 <*> lattice e2
                                <*> lattice e
  lattice e                   = [e]

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
  count (Lam _ e t)           = let c1 = count e
                                    c2 = count t
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
  count (Repeat _ e1 e2 e3)   = let c1 = count e1
                                    c2 = count e2
                                    c3 = count e3
                                in ((*) (fst c1 * fst c2) *** (+) (snd c1 + snd c2)) c3
  count _                     = (1,0)

  static (Op _ es)           = sum (map static es)
  static (If e1 e2 e3)       = static e1 + static e2 + static e3
  static (App e1 es)         = static e1 + sum (map static es)
  static (Lam _ e t)         = static t + static e
  static (GRef e)            = static e
  static (GDeRef e)          = static e
  static (GAssign e1 e2)     = static e1 + static e2
  static (MRef e)            = static e
  static (MDeRef e)          = static e
  static (MAssign e1 e2)     = static e1 + static e2
  static (GVect e1 e2)       = static e1 + static e2
  static (GVectRef e1 e2)    = static e1 + static e2
  static (GVectSet e1 e2 e3) = static e1 + static e2 + static e3
  static (MVect e1 e2)       = static e1 + static e2
  static (MVectRef e1 e2)    = static e1 + static e2
  static (MVectSet e1 e2 e3) = static e1 + static e2 + static e3
  static (Let e1 e2)         = sum (map static e1) + static e2
  static (Letrec e1 e2)      = sum (map static e1) + static e2
  static (As e t)            = static t + static e
  static (Begin e' e)        = static e + sum (map static e')
  static (Repeat _ e1 e2 e3) = static e1 + static e2 + static e3
  static _                   = 0

  countTypeLattice (Op _ es)           = concatMap countTypeLattice es
  countTypeLattice (If e1 e2 e3)       = countTypeLattice e1 ++ countTypeLattice e2 ++ countTypeLattice e3
  countTypeLattice (App e1 es)         = countTypeLattice e1 ++ concatMap countTypeLattice es
  countTypeLattice (Lam _ e' t)        = fromIntegral (fst $ count t):countTypeLattice e'
  countTypeLattice (GRef e')           = countTypeLattice e'
  countTypeLattice (GDeRef e')         = countTypeLattice e'
  countTypeLattice (GAssign e1 e2)     = countTypeLattice e1 ++ countTypeLattice e2
  countTypeLattice (MRef e')           = countTypeLattice e'
  countTypeLattice (MDeRef e')         = countTypeLattice e'
  countTypeLattice (MAssign e1 e2)     = countTypeLattice e1 ++ countTypeLattice e2
  countTypeLattice (GVect e1 e2)       = countTypeLattice e1 ++ countTypeLattice e2
  countTypeLattice (GVectRef e1 e2)    = countTypeLattice e1 ++ countTypeLattice e2
  countTypeLattice (GVectSet e1 e2 e3) = countTypeLattice e1 ++ countTypeLattice e2 ++ countTypeLattice e3
  countTypeLattice (MVect e1 e2)       = countTypeLattice e1 ++ countTypeLattice e2
  countTypeLattice (MVectRef e1 e2)    = countTypeLattice e1 ++ countTypeLattice e2
  countTypeLattice (MVectSet e1 e2 e3) = countTypeLattice e1 ++ countTypeLattice e2 ++ countTypeLattice e3
  countTypeLattice (Let e1 e2)         = foldr ((++) . countTypeLattice) [] e1 ++ countTypeLattice e2
  countTypeLattice (Letrec e1 e2)      = foldr ((++) . countTypeLattice) [] e1 ++ countTypeLattice e2
  countTypeLattice (As e' t)           = countTypeLattice e' ++ [fromIntegral $ fst $ count t]
  countTypeLattice (Begin e1 e2)       = concatMap countTypeLattice e1 ++ countTypeLattice e2
  countTypeLattice (Repeat _ e1 e2 e3) = countTypeLattice e1 ++ countTypeLattice e2 ++ countTypeLattice e3
  countTypeLattice _ = []

instance (Gradual e, Gradual t) => Gradual (Bind e t) where
  lattice (x,t,e) = (x,,) <$> lattice t <*> lattice e
  count (_,t,e)   =  let c1 = count e
                         c2 = count t
                     in ((*) (fst c1) *** (+) (snd c1)) c2
  static (_,t,e)  = static e + static t
  countTypeLattice (_,t,e') = fromIntegral (fst $ count t):countTypeLattice e'

instance Gradual Type where
  lattice (GRefTy t)    = Dyn:(GRefTy <$> lattice t)
  lattice (MRefTy t)    = Dyn:(MRefTy <$> lattice t)
  lattice (GVectTy t)   = Dyn:(GVectTy <$> lattice t)
  lattice (MVectTy t)   = Dyn:(MVectTy <$> lattice t)
  lattice (FunTy t1 t2) = Dyn:(FunTy <$> mapM lattice t1 <*> lattice t2)
  lattice (ArrTy t1 t2) = ArrTy <$> mapM lattice t1 <*> lattice t2
  lattice Dyn           = [Dyn]
  lattice t             = [Dyn,t]

  count (GRefTy t)      = let c = count t in (fst c + 1, snd c + 1)
  count (MRefTy t)      = let c = count t in (fst c + 1, snd c + 1)
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
  static (GRefTy t)    = 1 + static t
  static (MRefTy t)    = 1 + static t
  static (GVectTy t)   = 1 + static t
  static (MVectTy t)   = 1 + static t
  static (FunTy t1 t2) = 1 + sum (map static (t2:t1))
  static (ArrTy t1 t2) = sum (map static (t2:t1))
  static _             = 1

  countTypeLattice = undefined
