{-# LANGUAGE FlexibleContexts #-}

module Lang where

import Control.Applicative ((<|>))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState, get, put, modify)
import Data.CallStack (HasCallStack, callStack)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Val a =
  IntVal Int
  | BoolVal Bool
  | FnVal String a (Env a)
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

newtype Env a = Env { getEnv :: NonEmpty (Map String (Val a)) }
  deriving (Show, Eq)

pushEnv :: (MonadState (Env a) m) => m ()
pushEnv = do
  (Env ms) <- get
  put $ Env $ NE.cons Map.empty ms

popEnv :: (MonadState (Env a) m) => m ()
popEnv = do
  Env (_ :| ms) <- get
  put $ maybe (Env $ mempty :| []) Env $ NE.nonEmpty ms

insertEnv :: (MonadState (Env a) m) => String -> Val a -> m ()
insertEnv var val = modify $ insertEnv' var val

insertEnv' :: String -> Val a -> Env a -> Env a
insertEnv' var val (Env (m :| ms)) =
  Env $ (Map.insert var val m) :| ms

lookupEnv :: (MonadError String m, MonadState (Env a) m, HasCallStack) => String -> m (Val a)
lookupEnv var = do
  (Env ms) <- get
  maybe (throwError $ "Unbound variable " <> var <> show callStack) pure $ findIn (NE.toList ms) var
  where findIn :: [Map String (Val a)] -> String -> Maybe (Val a)
        findIn [] _ = Nothing
        findIn ms v = -- maybe (findIn ms v) pure $ Map.lookup v m
          foldr (\frame mVal -> mVal <|> Map.lookup v frame) Nothing ms

withEnv :: (MonadState (Env b) m) => Env b -> m a -> m a
withEnv env m = do
  currentEnv <- get
  put env
  res <- m
  put currentEnv
  pure res
