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
      parseExpr "(let zs (Cons 5 (Cons 4 (Cons 3 Nil))))" `shouldBe` EDestructLetBinding (IdPattern "zs") [] [EApp (EApp (EVar "Cons") $ ENum 5) $ EApp (EApp (EVar "Cons") $ ENum 4) $ EApp (EApp (EVar "Cons") $ ENum 3) $ EVar "Nil"]
      parseExpr "(let z (len zs))" `shouldBe` EDestructLetBinding (IdPattern "z") [] [EApp (EVar "len") $ EVar "zs"]
      parseExpr "(let g (λx y ⇒ (+ x y)))" `shouldBe` EDestructLetBinding (IdPattern "g") [] [ELambda [Named "x" Nothing, Named "y" Nothing] Nothing [EApp (EApp (EVar "+") $ EVar "x") $ EVar "y"]]
      parseExpr "(let res0 (g 3 3))" `shouldBe` EDestructLetBinding (IdPattern "res0") [] [EApp (EApp (EVar "g") $ ENum 3) $ ENum 3]
      parseExpr "(let id (λx ⇒ x))" `shouldBe` EDestructLetBinding (IdPattern "id") [] [ELambda [Named "x" Nothing] Nothing [EVar "x"]]
      parseExpr "(let res2 (id 3))" `shouldBe` EDestructLetBinding (IdPattern "res2") [] [EApp (EVar "id") $ ENum 3]
      parseExpr "(let res3 (id true))" `shouldBe` EDestructLetBinding (IdPattern "res3") [] [EApp (EVar "id") $ EBool True]
      parseExpr "(let [id (λx ⇒ x)] <(id 3) (id true)>)" `shouldBe` ELetBinding (IdPattern "id") (ELambda [Named "x" Nothing] Nothing [EVar "x"]) [(ETuple [EApp (EVar "id") (ENum 3), EApp (EVar "id") (EBool True)])]
      parseExpr "(+ 1 2 3)" `shouldBe` EApp (EApp (EApp (EVar "+") $ ENum 1) $ ENum 2) (ENum 3)