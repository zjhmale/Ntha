module InferSpec where

import Ast
import Type
import Infer
import TypeScope
import State (resetId, resetUniqueName)
import Control.Monad (foldM)
import Prologue
import qualified Text.PrettyPrint as PP
import qualified Data.Set as S
import Test.Hspec

runInferSpecCases :: [(Expr, String)] -> IO ()
runInferSpecCases exprExpectPairs = do
    assumps <- assumptions
    (_, types, expects) <- foldM (\(env, types, expects) (expr, expect) -> do
                                    (env', ty) <- analyze expr env S.empty
                                    return (env', types ++ [ty], expects ++ [expect]))
                                 (assumps, [], []) exprExpectPairs
    resetId
    resetUniqueName
    (map (PP.text . show) types) `shouldBe` map PP.text expects

failInferSpecCase :: Expr -> String -> IO ()
failInferSpecCase expr error = do
    assumps <- assumptions
    analyze expr assumps S.empty `shouldThrow` errorCall error
    resetId
    resetUniqueName

spec :: Spec
spec = describe "inference test" $
        it "should inference type of given term" $ do
          -- data List a = Cons a (List a) | Nil
          resetId
          resetUniqueName
          tvarA <- makeVariable
          let name = "List"
          let vars = [tvarA]
          let dataType = TOper name vars
          let consConstructor = TypeConstructor "Cons" [tvarA, TOper "List" [tvarA]]
          let nilConstructor = TypeConstructor "Nil" []
          let listData = EDataDecl "List" dataType vars [consConstructor, nilConstructor]
          (PP.text . show $ listData) `shouldBe` PP.text "data List α = Cons α [α] | Nil"
          let xs = EDestructLetBinding (IdPattern "xs") [] [(EVar "Nil")] -- let ys = Nil
          let ys = EDestructLetBinding (IdPattern "ys") [] [EApp (EApp (EVar "Cons") $ ENum 5) $ EVar "Nil"] -- let xs = Cons 5 Nil
          {-
            let len l =
              match l
                Nil => 0
                Cons h t => 1 + len t
          -}
          let len = EDestructLetBinding (IdPattern "len") [IdPattern "l"] [EPatternMatching (EVar "l") [Case (TConPattern "Nil" []) [ENum 0], Case (TConPattern "Cons" [IdPattern "h", IdPattern "t"]) [EApp (EApp (EVar "+") $ ENum 1) $ EApp (EVar "len") $ EVar "t"]]]
          let xy = EDestructLetBinding (IdPattern "xy") [] [ETuple [EApp (EVar "len") (EVar "xs"), EApp (EVar "len") (EVar"ys")]]
          let zs = EDestructLetBinding (IdPattern "zs") [] [EApp (EApp (EVar "Cons") $ ENum 5) $ EApp (EApp (EVar "Cons") $ ENum 4) $ EApp (EApp (EVar "Cons") $ ENum 3) $ EVar "Nil"]
          let z = EDestructLetBinding (IdPattern "z") [] [EApp (EVar "len") $ EVar "zs"]
          let g = EDestructLetBinding (IdPattern "g") [] [ELambda [Named "x" Nothing, Named "y" Nothing] Nothing [EApp (EApp (EVar "+") $ EVar "x") $ EVar "y"]]
          let res0 = EDestructLetBinding (IdPattern "res0") [] [EApp (EApp (EVar "g") $ ENum 3) $ ENum 3]
          let f = EDestructLetBinding (IdPattern "f") [] [ELambda [Named "x" (Just intT), Named "y" (Just intT), Named "z" (Just intT)] (Just intT) [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "x") $ EVar "y")) $ EVar "z"]]
          let ff = EDestructLetBinding (IdPattern "ff") [] [ELambda [Named "x" (Just intT), Named "y" (Just boolT), Named "z" (Just intT)] (Just intT) [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "x") $ EVar "y")) $ EVar "z"]]
          let res1 = EDestructLetBinding (IdPattern "res1") [] [EApp (EApp (EApp (EVar "f") $ ENum 8) $ ENum 2) $ ENum 3]
          let id = EDestructLetBinding (IdPattern "id") [] [ELambda [Named "x" Nothing] Nothing [EVar "x"]]
          let res2 = EDestructLetBinding (IdPattern "res2") [] [EApp (EVar "id") $ ENum 3]
          let res3 = EDestructLetBinding (IdPattern "res3") [] [EApp (EVar "id") $ EBool True]
          let fib = EDestructLetBinding (IdPattern "fib") [IdPattern "x"] [EPatternMatching (EVar "x") [Case (NumPattern 0) [ENum 0], Case (NumPattern 1) [ENum 1], Case WildcardPattern [EApp (EApp (EVar "+") $ EApp (EApp (EVar "-") $ EVar "x") $ ENum 1) $ EApp (EApp (EVar "-") $ EVar "x") $ ENum 2]]]
          -- let polymorphism here!!!
          let idpair = ELetBinding (IdPattern "id") (ELambda [Named "x" Nothing] Nothing [EVar "x"]) (ETuple [EApp (EVar "id") (ENum 3), EApp (EVar "id") (EBool True)])
          -- show up type variables need to be normalized
          let cases = [(listData, "[α]"),
                       (xs, "[γ]"),
                       (ys, "[Number]"),
                       (len, "[μ] → Number"),
                       (xy, "(Number * Number)"),
                       (zs, "[Number]"),
                       (z, "Number"),
                       (g, "Number → (Number → Number)"),
                       (res0, "Number"),
                       (f, "Number → (Number → (Number → Number))"),
                       (res1, "Number"),
                       (id, "η → η"),
                       (res2, "Number"),
                       (res3, "Boolean"),
                       (idpair, "(Number * Boolean)"),
                       (fib, "Number → Number")]
          runInferSpecCases cases
          failInferSpecCase ff "Type mismatch Boolean ≠ Number"
