module Base where

import Lang
import Data.List.NonEmpty (NonEmpty((:|)))
import Test.Hspec (Spec, it, shouldBe, describe)

langSpec :: String -> (Exp -> Either String Val) -> Spec
langSpec desc eval = do
  describe desc $ do
    describe "constant exps" $ do
      it "returns a number" $ do
        let v = eval $ IntExp 5
        v `shouldBe` Right (IntVal 5)
      it "returns a bool" $ do
        let v = eval $ BoolExp True
        v `shouldBe` Right (BoolVal True)
    describe "basic exps" $ do
      it "subtracts" $ do
        let v = eval $ DiffExp (IntExp 7) (IntExp 3)
        v `shouldBe` Right (IntVal 4)
      it "checks zero" $ do
        let v = eval $ ZeroExp (IntExp 0)
        v `shouldBe` Right (BoolVal True)
        let w = eval $ ZeroExp (IntExp 1)
        w `shouldBe` Right (BoolVal False)
      it "can if-then-else" $ do
        let v = eval $ IfExp (BoolExp True) (IntExp 1) (IntExp 0)
        v `shouldBe` Right (IntVal 1)
        let w = eval $ IfExp (BoolExp False) (IntExp 1) (IntExp 0)
        w `shouldBe` Right (IntVal 0)
    describe "let exps" $ do
      it "looks up variables" $ do
        let v = eval $ LetExp "x" (IntExp 2) (VarExp "x")
        v `shouldBe` Right (IntVal 2)
      it "substitutes variables in expressions" $ do
        let v = eval $
                  LetExp "x"
                    (IntExp 20)
                    (IfExp (ZeroExp (VarExp "x"))
                           (IntExp 1)
                           (DiffExp (VarExp "x") (IntExp 5)))
        v `shouldBe` Right (IntVal 15)
    describe "functions" $ do
      describe "binding and calling" $ do
        it "runs the identity" $ do
          let v = eval $
                    LetExp "id"
                    (FnExp (nes "x") (VarExp "x"))
                    (CallExp (VarExp "id") (nes $ IntExp 5))
          v `shouldBe` Right (IntVal 5)
        it "uncurries a function call" $ do
          let v = eval $
                    LetExp "minus"
                    (FnExp ("x" :| ["y"]) (DiffExp (VarExp "x") (VarExp "y")))
                    (CallExp (VarExp "minus") (IntExp 5 :| [IntExp 3]))
          v `shouldBe` Right (IntVal 2)
        it "curries a function call" $ do
          let v = eval $
                    LetExp "minus"
                    (FnExp ("x" :| ["y"]) (DiffExp (VarExp "x") (VarExp "y")))
                    (CallExp
                      (CallExp (VarExp "minus") (nes $ IntExp 5))
                      (nes $ IntExp 3))
          v `shouldBe` Right (IntVal 2)
        it "calls a recursive function" $ do
          let v = eval $
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
