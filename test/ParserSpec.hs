module ParserSpec where

import Ast
import Type
import State
import Parser
import qualified Text.PrettyPrint as PP
import Test.Hspec

spec :: Spec
spec = do
  describe "parser test" $
    it "should parse expressions" $ do
      tvarA <- makeVariable
      let name = "List"
      let vars = [tvarA]
      let dataType = TOper name vars
      let consConstructor = TypeConstructor "Cons" [tvarA, TOper "List" [tvarA]]
      let nilConstructor = TypeConstructor "Nil" []
      let listData = EDataDecl "List" dataType vars [consConstructor, nilConstructor]
      resetId
      resetUniqueName
      parseExpr "(data List a (Cons a (List a)) Nil)" `shouldBe` listData
      parseExpr "(≡ xs Nil)" `shouldBe` EDestructLetBinding (IdPattern "xs") [] [(EVar "Nil")]