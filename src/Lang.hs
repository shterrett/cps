{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Lang where

import Control.Applicative ((<|>))
import Control.Monad.Except (MonadError, throwError, runExceptT)
import Control.Monad.State (MonadState, get, put, modify, evalState)
import Data.CallStack (HasCallStack, callStack)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Val =
  IntVal Int
  | BoolVal Bool
  | FnVal String Exp Env
  deriving (Show, Eq)

data Exp =
  IntExp Int
  | BoolExp Bool
  | IfExp Exp Exp Exp
  | ZeroExp Exp
  | DiffExp Exp Exp
  | VarExp String
  | LetExp String Exp Exp -- bind body
  | FnExp (NonEmpty String) Exp -- vars body
  | FncExp String Exp -- var body
  | CallExp Exp (NonEmpty Exp) -- f args
  | CallcExp Exp Exp -- f arg
  | LetRecExp String (NonEmpty String) Exp Exp -- name args fn-body let-body
  deriving (Show, Eq)

newtype Env = Env { getEnv :: NonEmpty (Map String Val) }
  deriving (Show, Eq)

pushEnv :: (MonadState Env m) => m ()
pushEnv = do
  (Env ms) <- get
  put $ Env $ NE.cons Map.empty ms

popEnv :: Env -> Env
popEnv (Env base@(_ :| ms)) = Env $ maybe base id $ NE.nonEmpty ms

insertEnv :: (MonadState Env m) => String -> Val -> m ()
insertEnv var val = modify $ insertEnv' var val

insertEnv' :: String -> Val -> Env -> Env
insertEnv' var val (Env (m :| ms)) =
  Env $ (Map.insert var val m) :| ms

lookupEnv :: (MonadError String m, MonadState Env m, HasCallStack) => String -> m Val
lookupEnv var = do
  (Env ms) <- get
  maybe (throwError $ "Unbound variable " <> var <> show callStack) pure $ findIn (NE.toList ms) var
  where findIn :: [Map String Val] -> String -> Maybe Val
        findIn [] _ = Nothing
        findIn ms v = -- maybe (findIn ms v) pure $ Map.lookup v m
          foldr (\frame mVal -> mVal <|> Map.lookup v frame) Nothing ms

withEnv :: (MonadState Env m) => Env -> m a -> m a
withEnv env m = do
  currentEnv <- get
  put env
  res <- m
  put currentEnv
  pure res

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
  case fnExp of
    FnVal v body _ -> do
      newEnv <- pushEnv >> get
      let closure = insertEnv' name (FnVal v body closure) newEnv
      put closure
      eval letBody
    err -> typeError "letrec" e err

typeError :: (MonadError String m, Show e, Show err) => String -> e -> err -> m a
typeError ty e err =
  throwError $ "Expected " <> ty <> " in " <> show e <> " but got " <> show err

curryE :: NonEmpty String -> Exp -> Exp
curryE vs body = foldr ($) body $ FncExp <$> vs

uncurryE :: Exp -> NonEmpty Exp -> Exp
uncurryE fn args = foldl' CallcExp fn args

runEval :: Exp -> Either String Val
runEval = flip evalState (Env $ mempty :| []) . runExceptT . eval
