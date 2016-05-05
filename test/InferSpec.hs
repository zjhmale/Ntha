module InferSpec where

import Ast
import Type
import Infer
import TypeScope
import State (resetId, resetUniqueName)
import Control.Monad (foldM)
import qualified Text.PrettyPrint as PP
import qualified Data.Set as S
import Test.Hspec

runInferSpecCases :: [Expr] -> [String] -> IO ()
runInferSpecCases exprs expects = do
    assumps <- assumptions
    (_, types) <- foldM (\(env, types) expr -> do
                                        (env', ty) <- analyze expr env S.empty
                                        return (env', types ++ [ty]))
                        (assumps, []) exprs
    resetId
    resetUniqueName
    (map (PP.text . show) types) `shouldBe` map PP.text expects

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
          let xs = EDestructLetBinding (IdPattern "ys") [] [(EVar "Nil")] -- let ys = Nil
          let ys = EDestructLetBinding (IdPattern "xs") [] [EApp (EApp (EVar "Cons") $ ENum 5) $ EVar "Nil"] -- let xs = Cons 5 Nil
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
          -- show up type variables need to be normalized
          runInferSpecCases [listData, xs, ys, len, xy, zs, z] ["[α]", "[γ]", "[Number]", "[μ] → Number", "(Number * Number)", "[Number]", "Number"]
