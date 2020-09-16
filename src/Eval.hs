{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Eval where

import Lang
import Control.Monad.Except (MonadError, throwError, runExceptT)
import Control.Monad.State (MonadState, get, put, evalState)
import Data.CallStack (HasCallStack)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty(..))

eval :: (MonadError String m, MonadState Env m, HasCallStack) => Exp -> m Val
eval (IntExp i) = pure $ IntVal i
eval (BoolExp b) = pure $ BoolVal b
eval e@(IfExp tst t f) = do
  b <- eval tst >>= \case
        BoolVal b -> pure b
        err -> typeError "bool" e err
  if b
    then eval t
    else eval f
eval e@(ZeroExp nExp) = do
  n <- eval nExp >>= \case
        IntVal n -> pure n
        err -> typeError "int" e err
  pure $ BoolVal $ n == 0
eval e@(DiffExp e1 e2) = do
  n1 <- eval e1 >>= \case
        IntVal n -> pure n
        err -> typeError "int" e err
  n2 <- eval e2 >>= \case
        IntVal n -> pure n
        err -> typeError "int" e err
  pure $ IntVal (n1 - n2)
eval (VarExp v) = do
  lookupEnv v
eval (LetExp v bind body) = do
  pushEnv
  eval bind >>= insertEnv v
  eval body
eval (FnExp vs body) = eval $ curryE vs body
eval (FncExp v body) = do
  env <- get
  pure $ FnVal v body env
eval (CallExp f args) =
  eval $ uncurryE f args
eval e@(CallcExp f arg) = do
  fn <- eval f
  case fn of
    (FnVal v body env) -> do
      argVal <- eval arg
      withEnv env $ do
        pushEnv
        insertEnv v argVal
        eval body
    err -> typeError "function" e err
eval e@(LetRecExp name vars fnBody letBody) = do
  fnExp <- eval $ curryE vars fnBody
  (v, body) <- case fnExp of
    FnVal v body _ -> pure (v, body)
    err -> typeError "letrec" e err
  newEnv <- pushEnv >> get
  let closure = insertEnv' name (FnVal v body closure) newEnv
  put closure
  res <- eval letBody
  put newEnv >> popEnv
  pure res

typeError :: (MonadError String m, Show e, Show err) => String -> e -> err -> m a
typeError ty e err =
  throwError $ "Expected " <> ty <> " in " <> show e <> " but got " <> show err

curryE :: NonEmpty String -> Exp -> Exp
curryE vs body = foldr ($) body $ FncExp <$> vs

uncurryE :: Exp -> NonEmpty Exp -> Exp
uncurryE fn args = foldl' CallcExp fn args

runEval :: Exp -> Either String Val
runEval = flip evalState (Env $ mempty :| []) . runExceptT . eval
