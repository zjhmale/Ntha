module InferSpec where

import Ast
import Type
import Infer
import TypeScope
import State (resetId, resetUniqueName)
import Control.Monad (foldM)
import Prologue
import qualified Data.Map as M
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
spec = describe "inference test" $ do
        it "should infer type of ADT and pattern match expressions part1" $ do
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
          let xs = EDestructLetBinding (IdPattern "xs") [] [(EVar "Nil")]
          let ys = EDestructLetBinding (IdPattern "ys") [] [EApp (EApp (EVar "Cons") $ ENum 5) $ EVar "Nil"]
          let len = EDestructLetBinding (IdPattern "len") [IdPattern "l"] [EPatternMatching (EVar "l") [Case (TConPattern "Nil" []) [ENum 0], Case (TConPattern "Cons" [IdPattern "h", IdPattern "t"]) [EApp (EApp (EVar "+") $ ENum 1) $ EApp (EVar "len") $ EVar "t"]]]
          let xy = EDestructLetBinding (IdPattern "xy") [] [ETuple [EApp (EVar "len") (EVar "xs"), EApp (EVar "len") (EVar"ys")]]
          let zs = EDestructLetBinding (IdPattern "zs") [] [EApp (EApp (EVar "Cons") $ ENum 5) $ EApp (EApp (EVar "Cons") $ ENum 4) $ EApp (EApp (EVar "Cons") $ ENum 3) $ EVar "Nil"]
          let z = EDestructLetBinding (IdPattern "z") [] [EApp (EVar "len") $ EVar "zs"]
          runInferSpecCases [(listData, "[α]"),
                             (xs, "[α]"),
                             (ys, "[Number]"),
                             (len, "[α] → Number"),
                             (xy, "(Number * Number)"),
                             (zs, "[Number]"),
                             (z, "Number")]
        it "should infer type of ADT and pattern match expressions part2" $ do
          tvarB <- makeVariable
          let name2 = "Tree"
          let vars2 = [tvarB]
          let dataType2 = TOper name2 vars2
          let nullConstructor = TypeConstructor "Null" []
          let leafConstructor = TypeConstructor "Leaf" [tvarB]
          let nodeConstructor = TypeConstructor "Node" [dataType2, tvarB, dataType2]
          let treeData = EDataDecl name2 dataType2 vars2 [nullConstructor, leafConstructor, nodeConstructor]
          let t = EApp (EApp (EApp (EVar "Node") $ EApp (EVar "Leaf") $ ENum 5) $ ENum 4) $ EApp (EVar "Leaf") $ ENum 3
          runInferSpecCases [(treeData, "(Tree α)"),
                             (t, "(Tree Number)")]
        it "should infer type of ADT and pattern match expressions part3" $ do
          let name3 = "Ast"
          let dataType3 = TOper name3 []
          let numConstructor = TypeConstructor "Num" [intT]
          let addConstructor = TypeConstructor "Add" [dataType3, dataType3]
          let subConstructor = TypeConstructor "Sub" [dataType3, dataType3]
          let mulConstructor = TypeConstructor "Mul" [dataType3, dataType3]
          let divConstructor = TypeConstructor "Div" [dataType3, dataType3]
          let astData = EDataDecl name3 dataType3 [] [numConstructor, addConstructor, subConstructor, mulConstructor, divConstructor]
          let eval = EDestructLetBinding (IdPattern "eval") [IdPattern "n"] [EPatternMatching (EVar "n") [Case (TConPattern "Num" [IdPattern "a"]) [EVar "a"],Case (TConPattern "Add" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "+") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"],Case (TConPattern "Sub" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "-") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"],Case (TConPattern "Mul" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "*") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"],Case (TConPattern "Div" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "/") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"]]]
          let sym = EDestructLetBinding (IdPattern "sym") [] [EApp (EApp (EVar "Mul") (EApp (EApp (EVar "Add") $ EApp (EVar "Num") $ ENum 4) $ EApp (EVar "Num") $ ENum 3)) (EApp (EApp (EVar "Sub") $ EApp (EVar "Num") $ ENum 4) $ EApp (EVar "Num") $ ENum 1)]
          let result = EDestructLetBinding (IdPattern "result") [] [EApp (EVar "eval") $ EVar "sym"]
          runInferSpecCases [(astData, "Ast"),
                             (eval, "Ast → Number"),
                             (sym, "Ast"),
                             (result, "Number")]
        it "should infer type of ADT and pattern match expressions part4" $ do
          let name4 = "Oper"
          let dataType4 = TOper name4 []
          let addOperConstructor = TypeConstructor "Add" []
          let subOperConstructor = TypeConstructor "Sub" []
          let operData = EDataDecl name4 dataType4 [] [addOperConstructor, subOperConstructor]
          let name5 = "Expr"
          let dataType5 = TOper name5 []
          let numExprConstructor = TypeConstructor "Num" [intT]
          let appExprConstructor = TypeConstructor "App" [dataType4, dataType5, dataType5]
          let exprData = EDataDecl name5 dataType5 [] [numExprConstructor, appExprConstructor]
          let a = EDestructLetBinding (IdPattern "a") [] [EApp (EApp (EApp (EVar "App") $ EVar "Add") $ EApp (EVar "Num") $ ENum 5) $ EApp (EVar "Num") $ ENum 6]
          let eval1 = EDestructLetBinding (IdPattern "eval1") [IdPattern "e"] [EPatternMatching (EVar "e") [Case (TConPattern "Num" [IdPattern "n"]) [EVar "n"],Case (TConPattern "App" [IdPattern "o", IdPattern "e1", IdPattern "e2"]) [EPatternMatching (EVar "o") [Case (TConPattern "Add" []) [EApp (EApp (EVar "+") $ EApp (EVar "eval1") $ EVar "e1") $ EApp (EVar "eval1") $ EVar "e2"],Case (TConPattern "Sub" []) [EApp (EApp (EVar "-") $ EApp (EVar "eval1") $ EVar "e1") $ EApp (EVar "eval1") $ EVar "e2"]]]]]
          let eval2 = EDestructLetBinding (IdPattern "eval2") [IdPattern "e"] [EPatternMatching (EVar "e") [Case (TConPattern "Num" [IdPattern "n"]) [EVar "n"],Case (TConPattern "App" [TConPattern "Add" [], IdPattern "e1", IdPattern "e2"]) [EApp (EApp (EVar "+") $ EApp (EVar "eval2") $ EVar "e1") $ EApp (EVar "eval2") $ EVar "e2"],Case (TConPattern "App" [TConPattern "Sub" [], IdPattern "e1", IdPattern "e2"]) [EApp (EApp (EVar "-") $ EApp (EVar "eval2") $ EVar "e1") $ EApp (EVar "eval2") $ EVar "e2"]]]
          let res1 = EDestructLetBinding (IdPattern "res1") [] [EApp (EVar "eval1") $ EVar "a"]
          let res2 = EDestructLetBinding (IdPattern "res2") [] [EApp (EVar "eval2") $ EVar "a"]
          let simplify = EDestructLetBinding (IdPattern "simplify") [IdPattern "e"] [EPatternMatching (EVar "e") [Case (TConPattern "App" [TConPattern "Add" [], TConPattern "Num" [IdPattern "n"], IdPattern "e2"]) [EIf (EApp (EApp (EVar "=") $ EVar "n") $ ENum 0) [EVar "e2"] [EVar "e"]]]]
          let a2 = EDestructLetBinding (IdPattern "a2") [] [EApp (EApp (EApp (EVar "App") $ EVar "Add") $ EApp (EVar "Num") $ ENum 0) $ EApp (EVar "Num") $ ENum 6]
          let b = EDestructLetBinding (IdPattern "b") [] [EApp (EVar "simplify") $ EVar "a2"]
          runInferSpecCases [(operData, "Oper"),
                             (exprData, "Expr"),
                             (a, "Expr"),
                             (eval1, "Expr → Number"),
                             (eval2, "Expr → Number"),
                             (res1, "Number"),
                             (res2, "Number"),
                             (simplify, "Expr → Expr"),
                             (a2, "Expr"),
                             (b, "Expr")]
        it "should infer type of lambda expressions even with type annotations" $ do
          let g = EDestructLetBinding (IdPattern "g") [] [ELambda [Named "x" Nothing, Named "y" Nothing] Nothing [EApp (EApp (EVar "+") $ EVar "x") $ EVar "y"]]
          let res0 = EDestructLetBinding (IdPattern "res0") [] [EApp (EApp (EVar "g") $ ENum 3) $ ENum 3]
          let f = EDestructLetBinding (IdPattern "f") [] [ELambda [Named "x" (Just intT), Named "y" (Just intT), Named "z" (Just intT)] (Just intT) [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "x") $ EVar "y")) $ EVar "z"]]
          let ff = EDestructLetBinding (IdPattern "ff") [] [ELambda [Named "x" (Just intT), Named "y" (Just boolT), Named "z" (Just intT)] (Just intT) [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "x") $ EVar "y")) $ EVar "z"]]
          let res1 = EDestructLetBinding (IdPattern "res1") [] [EApp (EApp (EApp (EVar "f") $ ENum 8) $ ENum 2) $ ENum 3]
          let id = EDestructLetBinding (IdPattern "id") [] [ELambda [Named "x" Nothing] Nothing [EVar "x"]]
          let res2 = EDestructLetBinding (IdPattern "res2") [] [EApp (EVar "id") $ ENum 3]
          let res3 = EDestructLetBinding (IdPattern "res3") [] [EApp (EVar "id") $ EBool True]
          -- let polymorphism here!!!
          let idpair = ELetBinding (IdPattern "id") (ELambda [Named "x" Nothing] Nothing [EVar "x"]) [(ETuple [EApp (EVar "id") (ENum 3), EApp (EVar "id") (EBool True)])]
          let idpair2 = ELetBinding (IdPattern "id") (ELambda [Named "x" Nothing] Nothing [EVar "x"]) [ELetBinding (IdPattern "a") (ENum 3) [ELetBinding (IdPattern "b") (EApp (EApp (EVar "+") $ EVar "a") $ ENum 3) [(ETuple [EApp (EVar "id") (EVar "a"), EApp (EVar "id") (EVar "b")])]]]
          let f1 = EDestructLetBinding (IdPattern "f1") [] [ELambda [Named "x" (Just intT), Named "y" (Just intT), Named "z" (Just intT)] (Just intT) [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "x") $ EVar "y")) $ EVar "z"]]
          let f2 = EDestructLetBinding (IdPattern "f2") [] [ELambda [Named "x" Nothing, Named "y" Nothing, Named "z" Nothing] Nothing [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "x") $ EVar "y")) $ EVar "z"]]
          let f1res = EDestructLetBinding (IdPattern "f1res") [] [EApp (EApp (EApp (EVar "f1") $ ENum 8) $ ENum 2) $ ENum 3]
          let f2res = EDestructLetBinding (IdPattern "f2res") [] [EApp (EApp (EApp (EVar "f2") $ ENum 8) $ ENum 2) $ ENum 3]
          runInferSpecCases [(g, "Number → (Number → Number)"),
                             (res0, "Number"),
                             (f, "Number → (Number → (Number → Number))"),
                             (res1, "Number"),
                             (id, "α → α"),
                             (res2, "Number"),
                             (res3, "Boolean"),
                             (idpair, "(Number * Boolean)"),
                             (idpair2, "(Number * Number)"),
                             (f1, "Number → (Number → (Number → Number))"),
                             (f2, "Number → (Number → (Number → Number))"),
                             (f1res, "Number"),
                             (f2res, "Number")]
          failInferSpecCase ff "Type mismatch Boolean ≠ Number"
        it "should infer type of function definition, application and pattern match" $ do
          let fib = EDestructLetBinding (IdPattern "fib") [IdPattern "x"] [EPatternMatching (EVar "x") [Case (NumPattern 0) [ENum 0], Case (NumPattern 1) [ENum 1], Case WildcardPattern [EApp (EApp (EVar "+") $ EApp (EApp (EVar "-") $ EVar "x") $ ENum 1) $ EApp (EApp (EVar "-") $ EVar "x") $ ENum 2]]]
          let fib0 = EApp (EVar "fib") $ ENum 0
          let penultimate = EProgram [EDestructLetBinding (IdPattern "penultimate") [IdPattern "xs"] [EPatternMatching (EVar "xs") [Case (TConPattern "Nil" []) [ENum 0],
                                                                                                                                    Case (TConPattern "Cons" [WildcardPattern, TConPattern "Nil" []]) [ENum 0],
                                                                                                                                    Case (TConPattern "Cons" [IdPattern "a", TConPattern "Cons" [WildcardPattern, TConPattern "Nil" []]]) [EVar "a"],
                                                                                                                                    Case (TConPattern "Cons" [IdPattern "x", TConPattern "Cons" [IdPattern "y", IdPattern "t"]]) [EApp (EVar "penultimate") (EVar "t")]]]]
          let res4 = EDestructLetBinding (IdPattern "res4") [] [EApp (EVar "penultimate") (EList [ENum 1, ENum 2, ENum 3])]
          let map = EDestructLetBinding (IdPattern "map") [IdPattern "f", IdPattern "l"] [EPatternMatching (EVar "l") [Case (TConPattern "Cons" [IdPattern "h", IdPattern "t"]) [EApp (EApp (EVar "Cons") $ EApp (EVar "f") $ EVar "h") $ EApp (EApp (EVar "map") $ EVar "f") $ EVar "t"],Case (TConPattern "Nil" []) [EVar "Nil"]]]
          let l = EDestructLetBinding (IdPattern "l") [] [EList [ENum 1, ENum 2, ENum 3]]
          let l3 = EDestructLetBinding (IdPattern "l3") [] [EApp (EApp (EVar "map") $ ELambda [Named "x" Nothing] Nothing [EApp (EApp (EVar "=") $ EApp (EApp (EVar "%") $ EVar "x") $ ENum 2) $ ENum 0]) $ EVar "l"]
          let k = EDestructLetBinding (IdPattern "k") [IdPattern "x", IdPattern "y"] [EPatternMatching (ETuple [EVar "x", EVar "y"]) [Case (TuplePattern [NumPattern 0, NumPattern 0]) [ENum 0], Case WildcardPattern [ENum 1]]]
          let fact = EDestructLetBinding (IdPattern "fact") [IdPattern "n"] [EIf (EApp (EApp (EVar "≤") $ EVar "n") $ ENum 1) [ENum 1] [EApp (EApp (EVar "*") $ EVar "n") (EApp (EVar "fact") $ EApp (EApp (EVar "-") $ EVar "n") $ ENum 1)]]
          let f5 = EDestructLetBinding (IdPattern "f5") [] [EApp (EVar "fact") $ ENum 5]
          let comp = EDestructLetBinding (IdPattern "comp") [IdPattern "f", IdPattern "g", IdPattern "x"] [EApp (EVar "f") (EApp (EVar "g") (EVar "x"))]
          let fix = EDestructLetBinding (IdPattern "fix") [] [EApp (EApp (EVar "comp") $ EVar "inc") (EVar "dec")]
          let incdec = EDestructLetBinding (IdPattern "incdec") [] [EApp (EVar "fix") (ENum 5)]
          let len = EDestructLetBinding (IdPattern "len") [IdPattern "xs"] [EPatternMatching (EVar "xs") [Case (TConPattern "Nil" []) [ENum 0],Case (TConPattern "Cons" [WildcardPattern, IdPattern "t"]) [EApp (EApp (EVar "+") $ ENum 1) (EApp (EVar "len") $ EVar "t")]]]
          let lenl = EApp (EVar "len") $ EVar "l"
          let append = EDestructLetBinding (IdPattern "append") [IdPattern "x", IdPattern "xs"] [EApp (EApp (EVar "Cons") $ EVar "x") $ EVar "xs"]
          let l2 = EDestructLetBinding (IdPattern "l2") [] [EApp (EApp (EVar "append") $ ENum 0) $ EVar "l"]
          let patmat0 = EDestructLetBinding (IdPattern "patmat0") [] [EPatternMatching (ETuple [EStr "a", ENum 3]) [Case (IdPattern "a") [ETuple [EStr "ok", EVar "a"]]]]
          let patmat1 = EDestructLetBinding (IdPattern "patmat1") [] [EPatternMatching (ETuple [EStr "a", ENum 3]) [Case (TuplePattern [IdPattern "a", IdPattern "b"]) [ETuple [EStr "ok", EVar "a", EVar "b"]]]]
          let patmat2 = EDestructLetBinding (IdPattern "patmat2") [] [EPatternMatching (ETuple [EStr "a", ENum 3]) [Case (TuplePattern [IdPattern "a", WildcardPattern]) [ETuple [EStr "ok", EVar "a"]]]]
          runInferSpecCases [(fib, "Number → Number"),
                             (fib0, "Number"),
                             (penultimate, "[Number] → Number"),
                             (res4, "Number"),
                             (map, "(α → β) → ([α] → [β])"),
                             (l, "[Number]"),
                             (l3, "[Boolean]"),
                             (k, "Number → (Number → Number)"),
                             (fact, "Number → Number"),
                             (f5, "Number"),
                             (comp, "(β → γ) → ((α → β) → (α → γ))"),
                             (fix, "Number → Number"),
                             (incdec, "Number"),
                             (len, "[α] → Number"),
                             (lenl, "Number"),
                             (append, "α → ([α] → [α])"),
                             (l2, "[Number]"),
                             (patmat0, "([Char] * ([Char] * Number))"),
                             (patmat1, "([Char] * [Char] * Number)"),
                             (patmat2, "([Char] * [Char])")]
        it "should infer type of basic syntax element" $ do
          let xb = EDestructLetBinding (IdPattern "x") [] [EBool True]
          let d = EDestructLetBinding (IdPattern "d") [] [ETuple [ETuple [ENum 4, EBool True], ETuple [EStr "test", EChar 'c', ENum 45]]]
          let intsum = EApp (EApp (EVar "+") (EApp (EApp (EVar "+") (EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ ENum 1) $ ENum 2)) $ ENum 3)) $ ENum 4)) $ ENum 5
          let l = EDestructLetBinding (IdPattern "y") [] [EList [ENum 1, ENum 2, ENum 3]]
          let l2 = EDestructLetBinding (IdPattern "z") [] [EList []]
          let a = EDestructLetBinding (IdPattern "a") [] [EChar 'a']
          let s = EDestructLetBinding (IdPattern "s") [] [EStr "qdsfsdf"]
          let l3 = EDestructLetBinding (IdPattern "l") [] [EApp (EApp (EVar "Cons") $ ENum 1) $ EApp (EApp (EVar "Cons") $ ENum 2) $ EApp (EApp (EVar "Cons") $ ENum 3) $ EVar "Nil"]
          let profile = EDestructLetBinding (IdPattern "profile") [] [ERecord (M.fromList [("name", EStr "ntha"), ("age", ENum 12)])]
          let name = EAccessor (EVar "profile") "name"
          runInferSpecCases [(xb, "Boolean"),
                             (d, "((Number * Boolean) * ([Char] * Char * Number))"),
                             (intsum, "Number"),
                             (l, "[Number]"),
                             (l2, "[α]"),
                             (a, "Char"),
                             (s, "[Char]"),
                             (l3, "[Number]"),
                             (profile, "{age: Number, name: [Char]}"),
                             (name, "[Char]")]


