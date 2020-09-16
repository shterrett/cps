module CpsMSpec where

import Base
import CpsM (runEval)
import Test.Hspec (Spec)

spec :: Spec
spec = langSpec "continuation passing interpreter" runEval
