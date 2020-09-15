module CpsSpec where

import Base
import Cps (runEval)
import Test.Hspec (Spec)

spec :: Spec
spec = langSpec "continuation passing interpreter" runEval
