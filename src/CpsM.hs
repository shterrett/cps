{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module CpsM where

import Lang
import Control.Monad.Trans.Cont (ContT(..))
import Control.Monad.Cont (MonadCont(..))
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.State (MonadState, evalState)
import Data.CallStack (HasCallStack)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Eval as Eval

eval ::
  ( MonadCont m
  , MonadError String m
  , MonadState (Env Exp) m
  , HasCallStack
  )
  => Exp
  -> m (Val Exp)
eval = Eval.eval

runEval :: Exp -> Either String (Val Exp)
runEval = flip evalState (Env $ mempty :| []) . flip runContT pure . runExceptT . eval
