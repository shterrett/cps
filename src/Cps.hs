{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Cps where

import Lang
import Eval (typeError, uncurryE, curryE)
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.State (MonadState, get, put, evalState)
import Data.CallStack (HasCallStack)
import Data.List.NonEmpty (NonEmpty((:|)))

eval ::
  ( MonadError String m
  , MonadState Env m
  , HasCallStack
  )
  => Exp
  -> (Val -> m r)
  -> m r
eval (IntExp i) k = k $ IntVal i
eval (BoolExp i) k = k $ BoolVal i
eval (IfExp tst t f) k =
  eval tst $ \case
    BoolVal b -> if b then eval t k else eval f k
    e -> typeError "boolean" tst e
eval (ZeroExp e) k =
  eval e $ \case
      IntVal i -> if i == 0 then k (BoolVal True) else k (BoolVal False)
      err -> typeError "integer" e err
eval (DiffExp e1 e2) k =
  eval e1 $ \case
      IntVal i1 ->
        eval e2 $  \case
            IntVal i2 -> k (IntVal $ i1 - i2)
            err2 -> typeError "integer" e2 err2
      err1 -> typeError "integer" e1 err1
eval (VarExp var) k = lookupEnv var >>= k
eval (LetExp var bind body) k =
  eval bind $ \val -> do
    insertEnv var val
    eval body k
eval (FnExp vars body) k =
  eval (curryE vars body) k
eval (FncExp v body) k = do
  env <- get
  k $ FnVal v body env
eval (CallExp f xs) k =
  eval (uncurryE f xs) k
eval (CallcExp f x) k =
  eval x $ \argVal -> do
    eval f $ \case
      FnVal var body env -> do
        oldEnv <- get
        put env
        pushEnv
        insertEnv var argVal
        eval body $ \res -> do
          put oldEnv
          k res
      err -> typeError "function" f err
eval (LetRecExp name args fnBody letBody) k = do
  let fnExp = curryE args fnBody
  eval fnExp $ \case
    FnVal v body _ -> do
      env <- get
      let closure = insertEnv' name (FnVal v body closure) env
      put closure
      pushEnv
      eval letBody $ \res -> do
        put env
        k res
    err -> typeError "function" fnExp err

runEval :: Exp -> Either String Val
runEval = flip evalState (Env $ mempty :| []) . runExceptT . flip eval pure
