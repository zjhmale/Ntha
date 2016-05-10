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
      parseExpr "(let xs Nil)" `shouldBe` EDestructLetBinding (IdPattern "xs") [] [(EVar "Nil")]
      parseExpr "(let ys (Cons 5 Nil))" `shouldBe` EDestructLetBinding (IdPattern "ys") [] [EApp (EApp (EVar "Cons") $ ENum 5) $ EVar "Nil"]
      parseExpr "(ƒ len [l] (match l (Nil ⇒ 0) (Cons h t ⇒ (+ 1 (len t)))))" `shouldBe` EDestructLetBinding (IdPattern "len") [IdPattern "l"] [EPatternMatching (EVar "l") [Case (TConPattern "Nil" []) [ENum 0], Case (TConPattern "Cons" [IdPattern "h", IdPattern "t"]) [EApp (EApp (EVar "+") $ ENum 1) $ EApp (EVar "len") $ EVar "t"]]]
      parseExpr "(let xy <(len xs) (len ys)>)" `shouldBe` EDestructLetBinding (IdPattern "xy") [] [ETuple [EApp (EVar "len") (EVar "xs"), EApp (EVar "len") (EVar"ys")]]
      parseExpr "(+ 1 2 3)" `shouldBe` EApp (EApp (EApp (EVar "+") $ ENum 1) $ ENum 2) (ENum 3)