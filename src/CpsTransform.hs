{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CpsTransform where

import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.State (MonadState, get, put, evalState, modify)
import Data.CallStack (HasCallStack)
import Data.List.NonEmpty (NonEmpty((:|)))
import Eval (curryE, uncurryE, typeError)
import Lang

data SimpleExp =
  SIntExp Int
    | SBoolExp Bool
    | SVarExp String
    | SDiffExp SimpleExp SimpleExp
    | SZeroExp SimpleExp
    | SFnExp String CpsExp
    deriving (Show, Eq)

data CpsExp =
  CpsSimple SimpleExp
    | CpsLet String SimpleExp CpsExp
    | CpsLetRec String (NonEmpty String) CpsExp CpsExp
    | CpsIf SimpleExp CpsExp CpsExp
    | CpsCall SimpleExp SimpleExp
    | CpsCallK SimpleExp SimpleExp SimpleExp
    deriving (Show, Eq)

cpsTransform ::
  ( MonadState Int m
  )
  => Exp
  -> SimpleExp
  -> m CpsExp
cpsTransform (IntExp i) k = pure $ CpsCall k (SIntExp i)
cpsTransform (BoolExp b) k = pure $ CpsCall k (SBoolExp b)
cpsTransform (VarExp v) k = pure $ CpsCall k (SVarExp v)
cpsTransform (DiffExp e1 e2) k = do
  e1Var <- nextVar
  e2Var <- nextVar
  let k2 = SFnExp e2Var $ CpsCall k (SDiffExp (SVarExp e1Var) (SVarExp e2Var))
  e2' <- cpsTransform e2 k2
  let k1 = SFnExp e1Var e2'
  cpsTransform e1 k1
cpsTransform (IfExp tst t f) k = do
  testVar <- nextVar
  t' <- cpsTransform t k
  f' <- cpsTransform f k
  let ifK = SFnExp testVar $ CpsIf (SVarExp testVar) t' f'
  cpsTransform tst ifK
cpsTransform (ZeroExp e) k = do
  var <- nextVar
  cpsTransform e $ SFnExp var $ CpsCall k $ SZeroExp (SVarExp var)
cpsTransform (LetExp var bind body) k = do
  bindVar <- nextVar
  body' <- cpsTransform body k
  let bindK = SFnExp bindVar $ CpsLet var (SVarExp bindVar) body'
  cpsTransform bind bindK
cpsTransform (FnExp vars body) k = cpsTransform (curryE vars body) k
cpsTransform (FncExp var body) k = do
  body' <- cpsTransform body idK
  pure $ CpsCall k (SFnExp var body')
cpsTransform (CallExp f args) k = cpsTransform (uncurryE f args) k
cpsTransform (CallcExp f arg) k = do
  argVar <- nextVar
  fVar <- nextVar
  let callK = SFnExp argVar $ CpsCallK k (SVarExp fVar) (SVarExp argVar)
  arg' <- cpsTransform arg callK
  let callF = SFnExp fVar arg'
  cpsTransform f $ callF
cpsTransform (LetRecExp name args fnBody letBody) k = do
  fnBody' <- cpsTransform fnBody idK
  letBody' <- cpsTransform letBody k
  pure $ CpsLetRec name args fnBody' letBody'

idK :: SimpleExp
idK = SFnExp "cps-id-k-var" (CpsSimple $ SVarExp "cps-id-k-var")

nextVar :: (MonadState Int m) => m String
nextVar = do
  i <- get
  modify (+1)
  pure $ "cps-k-var-" <> show i

evalSimple ::
  ( MonadError String m
  , MonadState (Env CpsExp) m
  , HasCallStack
  )
  => SimpleExp
  -> m (Val CpsExp)
evalSimple (SIntExp i) = pure $ IntVal i
evalSimple (SBoolExp b) = pure $ BoolVal b
evalSimple (SVarExp var) = lookupEnv var
evalSimple (SDiffExp x y) = do
  x' <- evalSimple x >>= \case
          IntVal v -> pure v
          e -> typeError "int" x e
  y' <- evalSimple y >>= \case
          IntVal v -> pure v
          e -> typeError "int" x e
  pure $ IntVal $ x' - y'
evalSimple (SZeroExp z) = do
  i <- evalSimple z >>= \case
        IntVal v -> pure v
        e -> typeError "int" z e
  pure $ BoolVal $ i == 0
evalSimple (SFnExp var body) =
  FnVal var body <$> get

eval ::
  ( MonadError String m
  , MonadState (Env CpsExp) m
  , HasCallStack
  )
  => CpsExp
  -> m (Val CpsExp)
eval (CpsSimple se) = evalSimple se
eval (CpsLet var bind body) = do
  bindVal <- evalSimple bind
  pushEnv *> insertEnv var bindVal
  eval body <* popEnv
eval e@(CpsLetRec fnName vars fnBody letBody) = do
  fnExp <- evalSimple (curryCpsE vars fnBody)
  (v, body) <-case fnExp of
                FnVal v body _ -> pure (v, body)
                err -> typeError "letrec" e err
  newEnv <- pushEnv >> get
  let closure = insertEnv' fnName (FnVal v body closure) newEnv
  put closure
  res <- eval letBody
  put newEnv >> popEnv
  pure res
eval e@(CpsIf tst t f) = do
  tstVal <- evalSimple tst >>= \case
              BoolVal b -> pure b
              err -> typeError "bool" e err
  if tstVal
    then eval t
    else eval f
eval e@(CpsCall fn arg) = do
  (v, b, closure) <- evalSimple fn >>= \case
                  FnVal v b closure -> pure (v, b, closure)
                  err -> typeError "function" e err
  argVal <- evalSimple arg
  currEnv <- get
  put closure
  pushEnv >> insertEnv v argVal
  res <- eval b
  put currEnv
  pure res
eval e@(CpsCallK k' fn arg) = do
  x <- evalSimple arg
  (fnV, fnBody, closure) <- evalSimple fn >>= \case
                      FnVal var body closure -> pure (var, body, closure)
                      err -> typeError "function" e err
  currEnv <- get
  put closure
  pushEnv >> insertEnv fnV x
  fnRes <- eval fnBody
  (kV, kBody, kClosure) <- evalSimple k' >>= \case
                            FnVal var body kClosure -> pure (var, body, kClosure)
                            err -> typeError "function" e err
  put kClosure
  pushEnv >> insertEnv kV fnRes
  res <- eval kBody
  put currEnv
  pure res

curryCpsE :: NonEmpty String -> CpsExp -> SimpleExp
curryCpsE (v :| []) body = SFnExp v body
curryCpsE (v :| vs) body = foldr ($) (SFnExp v body) $ (\var -> SFnExp var . CpsSimple) <$> vs

runEval :: Exp -> Either String (Val CpsExp)
runEval e =
  let
    cpsExp = evalState (cpsTransform e idK) 0
  in
    flip evalState (Env $ mempty :| []) . runExceptT $ eval cpsExp
