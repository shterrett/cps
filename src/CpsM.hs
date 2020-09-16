{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module CpsM where

import Lang
import Eval (typeError, uncurryE, curryE)
import Control.Monad.Trans.Cont (ContT(..))
import Control.Monad.Cont (MonadCont(..))
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.State (MonadState, get, put, evalState)
import Data.CallStack (HasCallStack)
import Data.List.NonEmpty (NonEmpty((:|)))

eval ::
  ( MonadCont m
  , MonadError String m
  , MonadState Env m
  , HasCallStack
  )
  => Exp
  -> m Val
eval (IntExp i) = pure $ IntVal i
eval (BoolExp i) = pure $ BoolVal i
eval (IfExp tst t f) =
  eval tst >>= \case
    BoolVal b -> if b then eval t else eval f
    e -> typeError "boolean" tst e
eval (ZeroExp e) =
  eval e >>= \case
      IntVal i -> if i == 0 then pure (BoolVal True) else pure (BoolVal False)
      err -> typeError "integer" e err
eval (DiffExp e1 e2) = do
  i1 <- eval e1 >>= \case
          IntVal x -> pure x
          err -> typeError "integer" e1 err
  i2 <- eval e2 >>= \case
          IntVal x -> pure x
          err -> typeError "integer" e2 err
  pure . IntVal $ i1 - i2
eval (VarExp var) = lookupEnv var
eval (LetExp var bind body) = do
  val <- eval bind
  insertEnv var val
  eval body
eval (FnExp vars body) = eval (curryE vars body)
eval (FncExp v body) = do
  env <- get
  pure $ FnVal v body env
eval (CallExp f xs) = eval (uncurryE f xs)
eval (CallcExp f x) = do
  argVal <- eval x
  (var, body, env) <- eval f >>= \case
      FnVal var body env -> pure (var, body, env)
      err -> typeError "function" f err
  oldEnv <- get
  put env
  pushEnv
  insertEnv var argVal
  res <- eval body
  put oldEnv
  pure res
eval (LetRecExp name args fnBody letBody) = do
  let fnExp = curryE args fnBody
  (v, body) <- eval fnExp >>= \case
    FnVal v body _ -> pure (v, body)
    err -> typeError "function" fnExp err
  env <- get
  let closure = insertEnv' name (FnVal v body closure) env
  put closure
  pushEnv
  res <- eval letBody
  put env
  pure res

runEval :: Exp -> Either String Val
runEval = flip evalState (Env $ mempty :| []) . flip runContT pure . runExceptT . eval
