module CpsTransformSpec where

import Base
import CpsTransform (runEval)
import Test.Hspec (Spec)

spec :: Spec
spec = langSpec "continuation passing transform" runEval
