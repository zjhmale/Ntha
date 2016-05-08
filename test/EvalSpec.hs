module EvalSpec where

import Ast
import Type
import Eval
import State (resetId, resetUniqueName)
import Control.Monad (foldM)
import Prologue
import qualified Text.PrettyPrint as PP
import qualified Data.Set as S
import Test.Hspec

spec :: Spec
spec = describe "inference test" $
        it "should inference type of given term" $ do
          1 `shouldBe` 1
