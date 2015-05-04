{-# LANGUAGE FlexibleContexts, TupleSections, LambdaCase #-}

-- TODO: clean up the error handling stuff

module TRecon(teval) where

import qualified Data.IntMap as M
import qualified Data.Map.Lazy as L
import Control.Monad.Error (Error(..), ErrorT, MonadError, throwError, runErrorT)
import Control.Monad.Reader (ReaderT, MonadReader, asks, local, runReaderT)
import Control.Monad (liftM)

import Syntax

-- Create a fresh type variable
genTVar :: Substitution -> (Type,Substitution)
genTVar (Substitution n s) = (TVar n,Substitution (succ n) s)

-- hypothetical reasoning
tvsub :: Substitution -> Type -> Type
tvsub tve (FunTy t1 t2) = FunTy (tvsub tve t1) $ tvsub tve t2
tvsub tve (TVar v) | Just t <- lookupSub tve v = tvsub tve t
tvsub _ t = t

-- shallow substitution
tvchase :: Substitution -> Type -> Type
tvchase tve (TVar v) | Just t <- lookupSub tve v = tvchase tve t
tvchase _ t = t

unify :: Type -> Type -> Substitution -> Either String Substitution
unify t1 t2 tve = unify' (tvchase tve t1) (tvchase tve t2) tve

unify' :: Type -> Type -> Substitution -> Either String Substitution
unify' IntTy IntTy = Right
unify' BoolTy BoolTy = Right
unify' (FunTy t1a t1r) (FunTy t2a t2r) = either Left (unify t1r t2r) . unify t1a t2a
unify' (TVar v1) t2 = unifyv v1 t2
unify' t1 (TVar v2) = unifyv v2 t1
unify' t1 t2 = const $ Left $ "constant mismatch:" ++ show t1 ++ "and" ++ show t2

-- Unify a free variable v1 with t2
unifyv :: TVName -> Type -> Substitution -> Either String Substitution
unifyv v1 (TVar v2) tve =
    if v1 == v2 then Right tve
       else Right (extendSub tve (v1,TVar v2)) -- record new constraint
unifyv v1 t2 tve = if isFree v1 t2 tve
                   then Left $ "isFree check:" ++ show (TVar v1) ++ "in" ++ show (tvsub tve t2)
                   else Right $ extendSub tve (v1,t2)

-- The isFree check: if v appears free in t
isFree :: TVName -> Type -> Substitution -> Bool
isFree _ IntTy _ = False
isFree _ BoolTy _ = False
isFree v (RefTy t) tve = isFree v t tve
isFree v (FunTy t1 t2) tve = isFree v t1 tve || isFree v t2 tve
isFree v (TVar v2) tve =
    case lookupSub tve v2 of
         Nothing -> v == v2
         Just t  -> isFree v t tve

data TrErr =
  UnboundedVar Name
  | IllTypedIfExp String
  | UnknownError String

instance Error TrErr where
  strMsg = UnknownError

instance Show TrErr where
  show (UnboundedVar v) = "Unbounded variable " ++ v
  show (IllTypedIfExp s) = s

data TrGamma = TrGamma {tyCtx :: L.Map Name Type, sub :: Substitution}
type TrMonad = ReaderT TrGamma (ErrorT TrErr IO)

-- | Look for the type of a variable in the context
-- throwing an error if the name doesn't exist.
lookupTy :: (MonadReader TrGamma m, MonadError TrErr m) => Name -> m Type
lookupTy v = asks tyCtx >>= \c -> maybe err return $ L.lookup v c
  where err = throwError $ UnboundedVar v

-- | Extend the context with a new binding.
extendTy :: (MonadReader TrGamma m) => (Name, Type) -> m a -> m a
extendTy (x,t) = local $ \m@(TrGamma {tyCtx = cs}) -> m {tyCtx = L.insert x t cs}

lookupSub :: Substitution -> TVName -> Maybe Type
lookupSub (Substitution _ s) v = M.lookup v s

extendSub :: Substitution -> (TVName,Type) -> Substitution
extendSub (Substitution c s) (tv,t) = Substitution c $ M.insert tv t s

extendSub1 :: (MonadReader TrGamma m) => (TVName, Type) -> m a -> m a
extendSub1 (x,t) = local $ \m@(TrGamma {sub = Substitution c s}) -> m {sub = Substitution c $ M.insert x t s}

resetSub :: (MonadReader TrGamma m) => Substitution -> m a -> m a
resetSub s = local $ \m -> m {sub = s}

-- Type reconstruction
teval' :: Exp1 -> TrMonad (Exp2,Type,Substitution)
teval' (Var1 x) = lookupTy x >>= \t -> liftM (Var2 x,t,) $ asks sub
teval' (N1 n) = liftM (N2 n,IntTy,) $ asks sub
teval' (B1 b) = liftM (B2 b,BoolTy,) $ asks sub
teval' (Op1 op e) = teval' e >>= \(e',t,tve) ->
  case unify t IntTy tve of
   Right tve' -> return (Op2 op e',IntTy,tve')
   Left err -> error err
teval' (If1 e1 e2 e3) =
  teval' e1 >>= \(e1',t1,tve1) ->
  resetSub tve1 (teval' e2) >>= \(e2',t2,tve2) ->
  resetSub tve2 (teval' e3) >>= \(e3',t3,tve3) ->
  case unify t1 BoolTy tve3 of
   Right tve -> case unify t2 t3 tve of
                 Right tve' -> return (If2 e1' e2' e3',t2,tve')
                 Left err -> throwError $ IllTypedIfExp $ "Unification failed. Branches of If have different types: " ++ err
   Left err -> error $ "Trying to check non-boolean condition: " ++ err
teval' (App1 e1 e2) =
  teval' e1 >>= \(e1',t1,tve1) ->
  resetSub tve1 (teval' e2) >>= \(e2',t2,tve2) ->
  let (t1r,tve3) = genTVar tve2
  in case unify t1 (FunTy t2 t1r) tve3 of
      Right tve -> return (App2 e1' e2',t1r,tve)
      Left err  -> error err
teval' (Lam1 x e) = asks sub >>= \tve0 ->
    let (tv,tve1) = genTVar tve0
    in extendTy (x,tv) (resetSub tve1 (teval' e)) >>= \(e',te,tve2) ->
    return (Lam2 (x,tv) (e',te), FunTy tv te,tve2)
teval' (Ref1 e) =
  teval' e >>= \(e',t,tve) -> return (Ref2 e',RefTy t,tve)
teval' (DeRef1 e) =
  teval' e >>= \(e',t, tve1) ->
    let  (tv,tve2) = genTVar tve1
    in case unify t (RefTy tv) tve2 of
       Right tve -> return (DeRef2 e',tv,tve)
       Left err -> error err
teval' (Assign1 e1 e2) =
  teval' e1 >>= \(e1',t1, tve1) ->
  resetSub tve1 (teval' e2) >>= \(e2',t2,tve2) ->
  case unify t1 (RefTy t2) tve2 of
   Right tve -> return (Assign2 e1' e2',t2,tve)
   Left err -> error err
teval' (Let1 x e1 e2) =
  teval' e1 >>= \(e1',t1, tve1) ->
  extendTy (x,t1) (resetSub tve1 (teval' e2)) >>= \(e2',t2,tve2) ->
  return (Let2 (x,t1,e1') e2',t2,tve2)

runTrMonad :: TrMonad a -> TrGamma -> IO (Either TrErr a)
runTrMonad m = runErrorT . runReaderT m

-- another pass to resolve local type variables, if any
loctvsub :: Exp2 -> Substitution -> Exp2
loctvsub e@(N2 _) _ = e
loctvsub e@(B2 _) _ = e
loctvsub (Op2 op e) s = Op2 op $ loctvsub e s
loctvsub (If2 e1 e2 e3) s = If2 (loctvsub e1 s) (loctvsub e2 s) $ loctvsub e3 s
loctvsub e@(Var2 _) _ = e
loctvsub (App2 e1 e2) s = App2 (loctvsub e1 s) $ loctvsub e2 s
loctvsub (Ref2 e) s = Ref2 $ loctvsub e s
loctvsub (DeRef2 e) s = DeRef2 $ loctvsub e s
loctvsub (Assign2 e1 e2) s = Assign2 (loctvsub e1 s) $ loctvsub e2 s
loctvsub (Let2 (x,t,e1) e2) s = Let2 (x,tvsub s t,loctvsub e1 s) $ loctvsub e2 s
loctvsub (Lam2 (x,t1) (e,t2)) s = Lam2 (x,tvsub s t1) (loctvsub e s, tvsub s t2)

-- Resolve all type variables, as far as possible
teval :: Exp1 -> IO (Either TrErr (Exp2, Type))
teval e = runTrMonad (teval' e) TrGamma {tyCtx = L.empty, sub = Substitution 0 M.empty} >>=
          \case Right (e',t,tve) -> return $ Right (loctvsub e' tve,tvsub tve t)
