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
    (_, types) <- foldM (\(env, types) expr -> do
                                        (env', ty) <- analyze expr env S.empty
                                        return (env', types ++ [ty]))
                        (createEmptyScope, []) exprs
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
          let instr1 = EDestructLetBinding (IdPattern "ys") [] [(EVar "Nil")] -- let ys = Nil
          let instr2 = EDestructLetBinding (IdPattern "xs") [] [EApp (EApp (EVar "Cons") $ ENum 5) $ EVar "Nil"] -- let xs = Cons 5 Nil
          runInferSpecCases [listData, instr1, instr2] ["[α]", "[γ]", "[Number]"] -- show up type variables need to be normalized
