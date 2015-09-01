{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Annotizer where

import System.Directory (createDirectoryIfMissing)

import Syntax
import CodeGen

class Gradual p where
  lattice :: p -> [p]

instance Gradual L1 where
  lattice (s,e) = map (s,) $ lattice e

instance Gradual Exp1 where
  lattice (Op1 op es) = Op1 op <$> mapM lattice es
  lattice (If1 e1 e2 e3) = If1 <$> lattice e1 <*> lattice e2 <*> lattice e3
  lattice (App1 e1 es) = App1 <$> lattice e1 <*> mapM lattice es 
  lattice (Lam1 args e t) = Lam1 <$> mapM lattice args <*> lattice e <*> lattice t
  lattice (GRef1 e) = GRef1 <$> lattice e
  lattice (GDeRef1 e) = GDeRef1 <$> lattice e
  lattice (GAssign1 e1 e2) = GAssign1 <$> lattice e1 <*> lattice e2
  lattice (MRef1 e) = MRef1 <$> lattice e
  lattice (MDeRef1 e) = MDeRef1 <$> lattice e
  lattice (MAssign1 e1 e2) = MAssign1 <$> lattice e1 <*> lattice e2
  lattice (GVect1 e1 e2) = GVect1 <$> lattice e1 <*> lattice e2
  lattice (GVectRef1 e1 e2) = GVectRef1 <$> lattice e1 <*> lattice e2
  lattice (GVectSet1 e1 e2 e3) = GVectSet1 <$> lattice e1 <*> lattice e2 <*> lattice e3
  lattice (MVect1 e1 e2) = MVect1 <$> lattice e1 <*> lattice e2
  lattice (MVectRef1 e1 e2) = MVectRef1 <$> lattice e1 <*> lattice e2
  lattice (MVectSet1 e1 e2 e3) = MVectSet1 <$> lattice e1 <*> lattice e2 <*> lattice e3
  lattice (Let1 e1 e2) = Let1 <$> mapM lattice e1 <*> lattice e2
  lattice (Letrec1 e1 e2) = Letrec1 <$> mapM lattice e1 <*> lattice e2
  lattice (As1 e t) = As1 <$> lattice e <*> lattice t
  lattice (Begin1 e' e) = Begin1 <$> mapM lattice e' <*> lattice e
  lattice (Repeat1 i e1 e2 e) = Repeat1 i <$> lattice e1 <*> lattice e2 <*> lattice e
  lattice e = [e]

instance Gradual Arg where
  lattice (x,t) = (x,) <$> lattice t

instance Gradual Bind where
  lattice (x,t,e) =  (x,,) <$> lattice t <*> lattice e

instance Gradual Type where
  lattice (GRefTy t) = Dyn: (GRefTy <$> lattice t)
  lattice (MRefTy t) = Dyn: (MRefTy <$> lattice t)
  lattice (GVectTy t) = Dyn: (GVectTy <$> lattice t)
  lattice (MVectTy t) = Dyn: (MVectTy <$> lattice t)
  lattice (FunTy t1 t2) = Dyn: (FunTy <$> mapM lattice t1 <*> lattice t2)
  lattice t = [t,Dyn]

nAnnotize :: L1 -> IO ()
nAnnotize e = let testDirName = "test/" in createDirectoryIfMissing False testDirName *>
  mapWrite 0 testDirName (map schmlCodGen $ lattice e)
  where mapWrite _ _ [] = return ()
        mapWrite n p (s:s') = writeFile (p ++ show n ++ ".schml") (s ++ "\n") *> mapWrite (n+1) p s'
