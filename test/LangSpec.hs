module LangSpec where

import Data.List.NonEmpty (NonEmpty((:|)))
import Lang (Exp(..), Val(..), runEval, curryE, uncurryE)
import Test.Hspec (Spec, it, shouldBe, describe)

spec :: Spec
spec =
  describe "interpreter" $ do
    describe "constant exps" $ do
      it "returns a number" $ do
        let v = runEval $ IntExp 5
        v `shouldBe` Right (IntVal 5)
      it "returns a bool" $ do
        let v = runEval $ BoolExp True
        v `shouldBe` Right (BoolVal True)
    describe "basic exps" $ do
      it "subtracts" $ do
        let v = runEval $ DiffExp (IntExp 7) (IntExp 3)
        v `shouldBe` Right (IntVal 4)
      it "checks zero" $ do
        let v = runEval $ ZeroExp (IntExp 0)
        v `shouldBe` Right (BoolVal True)
        let w = runEval $ ZeroExp (IntExp 1)
        w `shouldBe` Right (BoolVal False)
      it "can if-then-else" $ do
        let v = runEval $ IfExp (BoolExp True) (IntExp 1) (IntExp 0)
        v `shouldBe` Right (IntVal 1)
        let w = runEval $ IfExp (BoolExp False) (IntExp 1) (IntExp 0)
        w `shouldBe` Right (IntVal 0)
    describe "let exps" $ do
      it "looks up variables" $ do
        let v = runEval $ LetExp "x" (IntExp 2) (VarExp "x")
        v `shouldBe` Right (IntVal 2)
      it "substitutes variables in expressions" $ do
        let v = runEval $
                  LetExp "x"
                    (IntExp 20)
                    (IfExp (ZeroExp (VarExp "x"))
                           (IntExp 1)
                           (DiffExp (VarExp "x") (IntExp 5)))
        v `shouldBe` Right (IntVal 15)
    describe "functions" $ do
      describe "currying" $ do
        it "curries a function" $ do
          let cfn = curryE ("w" :| ["x", "y", "z"]) (IntExp 5)
          cfn `shouldBe`
            (FncExp "w"
              (FncExp "x"
                (FncExp "y"
                  (FncExp "z"
                    (IntExp 5)))))
        it "curries a function call" $ do
          let cfn = curryE ("w" :| ["x", "y", "z"]) (IntExp 5)
              ccall = uncurryE cfn $ IntExp 1 :| [IntExp 2, IntExp 3, IntExp 4]
          ccall `shouldBe`
            (CallcExp
              (CallcExp
                (CallcExp
                  (CallcExp
                    cfn
                  $ IntExp 1)
                $ IntExp 2)
              $ IntExp 3)
            $ IntExp 4)
      describe "binding and calling" $ do
        it "runs the identity" $ do
          let v = runEval $
                    LetExp "id"
                    (FnExp (nes "x") (VarExp "x"))
                    (CallExp (VarExp "id") (nes $ IntExp 5))
          v `shouldBe` Right (IntVal 5)
        it "uncurries a function call" $ do
          let v = runEval $
                    LetExp "minus"
                    (FnExp ("x" :| ["y"]) (DiffExp (VarExp "x") (VarExp "y")))
                    (CallExp (VarExp "minus") (IntExp 5 :| [IntExp 3]))
          v `shouldBe` Right (IntVal 2)
        it "curries a function call" $ do
          let v = runEval $
                    LetExp "minus"
                    (FnExp ("x" :| ["y"]) (DiffExp (VarExp "x") (VarExp "y")))
                    (CallExp
                      (CallExp (VarExp "minus") (nes $ IntExp 5))
                      (nes $ IntExp 3))
          v `shouldBe` Right (IntVal 2)
        it "calls a recursive function" $ do
          let v = runEval $
                    LetRecExp "toZero"
                      (nes "x")
                      (IfExp (ZeroExp (VarExp "x"))
                             (VarExp "x")
                             (CallExp (VarExp "toZero")
                                      (nes $ DiffExp (VarExp "x") (IntExp 1))))
                      (CallExp (VarExp "toZero") (nes $ IntExp 3))
          v `shouldBe` Right (IntVal 0)


nes :: a -> NonEmpty a
nes = (:| [])
