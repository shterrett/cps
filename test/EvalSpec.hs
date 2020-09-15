module EvalSpec where

import Base (langSpec)
import Data.List.NonEmpty (NonEmpty((:|)))
import Lang (Exp(..))
import Eval (runEval, curryE, uncurryE)
import Test.Hspec (Spec, it, shouldBe, describe)

spec :: Spec
spec = do
  langSpec "straight interpreter" runEval
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
