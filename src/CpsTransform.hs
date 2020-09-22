{-# LANGUAGE FlexibleContexts #-}
  {-# LANGUAGE ScopedTypeVariables #-}

module CpsTransform where

import Control.Applicative (liftA2)
import Control.Monad ((>=>))
import Control.Monad.Except (MonadError, throwError, runExceptT)
import Control.Monad.State (MonadState, get, put, evalState)
import Eval (curryE, uncurryE)
import Data.CallStack (HasCallStack)
import Data.List.NonEmpty (NonEmpty)
import Lang (Env(..), Exp(..), Val(..))

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

cpsTransform :: forall m. (MonadError String m) => Exp -> SimpleExp -> m CpsExp
cpsTransform (IntExp i) k = pure $ CpsCall k (SIntExp i)
cpsTransform (BoolExp b) k = pure $ CpsCall k (SBoolExp b)
cpsTransform (VarExp v) k = pure $ CpsCall k (SVarExp v)
cpsTransform (DiffExp e1 e2) k = do
  let k2 = SFnExp "e2" $ CpsCall k (SDiffExp (SVarExp "e1") (SVarExp "e2"))
  e2' <- cpsTransform e2 k2
  let k1 = SFnExp "e2" e2'
  cpsTransform e1 k1
cpsTransform (IfExp tst t f) k = do
  t' <- cpsTransform t k
  f' <- cpsTransform f k
  let ifK = SFnExp "test" $ CpsIf (SVarExp "test") t' f'
  cpsTransform tst ifK
cpsTransform (ZeroExp e) k =
  cpsTransform e $ SFnExp "z" $ CpsCall k $ SZeroExp (SVarExp "z")
cpsTransform (LetExp var bind body) k = do
  body' <- cpsTransform body k
  let bindK = SFnExp "bind" $ CpsLet var (SVarExp "bind") body'
  cpsTransform bind bindK
cpsTransform (FnExp vars body) k = cpsTransform (curryE vars body) k
cpsTransform (FncExp var body) k = do
  body' <- cpsTransform body idK
  pure $ CpsCall k (SFnExp var body')
cpsTransform (CallExp f args) k = cpsTransform (uncurryE f args) k
cpsTransform (CallcExp f arg) k = do
  let callK = SFnExp "arg" $ CpsCallK k (SVarExp "f") (SVarExp "arg")
  arg' <- cpsTransform arg callK
  let callF = SFnExp "f" arg'
  cpsTransform f $ callF
cpsTransform (LetRecExp name args fnBody letBody) k = do
  fnBody' <- cpsTransform fnBody idK
  letBody' <- cpsTransform letBody k
  pure $ CpsLetRec name args fnBody' letBody'

idK :: SimpleExp
idK = SFnExp "x" (CpsSimple $ SVarExp "x")

evalSimple ::
  ( MonadError String m
  , MonadState Env m
  , HasCallStack
  )
  => SimpleExp
  -> m Val
evalSimple = undefined

eval ::
  ( MonadError String m
  , MonadState Env m
  , HasCallStack
  )
  => CpsExp
  -> m Val
eval = undefined
