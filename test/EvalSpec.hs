module EvalSpec where

import Ast
import Type
import Value
import Eval
import Prologue
import qualified Text.PrettyPrint as PP
import Test.Hspec

runEvalSpecCases :: [Expr] -> [Value] -> IO ()
runEvalSpecCases exprs expects = do
    let (_, vals) = foldl (\(env, vals) expr -> let (env', val) = eval expr env
                                               in (env', vals ++ [val]))
                          (builtins, []) exprs
    (map (PP.text . show) vals) `shouldBe` map (PP.text . show) expects

spec :: Spec
spec = describe "inference test" $
        it "should inference type of given term" $ do
          -- data List a = Cons a (List a) | Nil
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
          1 `shouldBe` 1
          --let g = EDestructLetBinding (IdPattern "g") [] [ELambda [Named "x" Nothing, Named "y" Nothing] Nothing [EApp (EApp (EVar "+") $ EVar "x") $ EVar "y"]]
          --let res0 = EDestructLetBinding (IdPattern "res0") [] [EApp (EApp (EVar "g") $ ENum 3) $ ENum 3]
          --let f = EDestructLetBinding (IdPattern "f") [] [ELambda [Named "x" (Just intT), Named "y" (Just intT), Named "z" (Just intT)] (Just intT) [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "x") $ EVar "y")) $ EVar "z"]]
          --let ff = EDestructLetBinding (IdPattern "ff") [] [ELambda [Named "x" (Just intT), Named "y" (Just boolT), Named "z" (Just intT)] (Just intT) [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "x") $ EVar "y")) $ EVar "z"]]
          --let res1 = EDestructLetBinding (IdPattern "res1") [] [EApp (EApp (EApp (EVar "f") $ ENum 8) $ ENum 2) $ ENum 3]
          --let id = EDestructLetBinding (IdPattern "id") [] [ELambda [Named "x" Nothing] Nothing [EVar "x"]]
          --let res2 = EDestructLetBinding (IdPattern "res2") [] [EApp (EVar "id") $ ENum 3]
          --let res3 = EDestructLetBinding (IdPattern "res3") [] [EApp (EVar "id") $ EBool True]
          -- show up type variables need to be normalized
          --runEvalSpecCases [listData, xs, ys, len, xy, zs, z]
                           --[VUnit, Adt "Nil" [], Adt "Cons" [VNum 5, Adt "Nil" []], VTuple [VNum 0, VNum 1], Adt "Cons" [VNum 5, Adt "Cons" [VNum 4, Adt "Cons" [VNum 3, Adt "Nil" []]]], VNum 3]
          --[listData, xs, ys, len, xy, zs, z, g, res0, f, res1, id, res2, res3]
          --["[α]", "[γ]", "[Number]", "[μ] → Number", "(Number * Number)", "[Number]", "Number", "Number → (Number → Number)", "Number", "Number → (Number → (Number → Number))", "Number", "η → η", "Number", "Boolean"]
          --failInferSpecCase ff "Type mismatch Boolean ≠ Number"
