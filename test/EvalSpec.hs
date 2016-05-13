{-# LANGUAGE UnicodeSyntax #-}

module EvalSpec where

import Ast
import Type
import Value
import Eval
import Prologue
import qualified Text.PrettyPrint as PP
import Test.Hspec

runEvalSpecCases :: [(Expr, Maybe Value)] -> IO ()
runEvalSpecCases exprExpects = do
    let (_, vals, expects) = foldl (\(env, vals, expects) (expr, expect) → let (env', val) = eval expr env
                                                                           in case expect of
                                                                                Just e -> (env', vals ++ [val], expects ++ [e])
                                                                                Nothing -> (env', vals, expects))
                                   (builtins, [], []) exprExpects
    (map (PP.text . show) vals) `shouldBe` map (PP.text . show) expects

spec :: Spec
spec = describe "evaluation test" $ do
        it "should get value of ADT and pattern match expressions part1" $ do
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
          runEvalSpecCases [(listData, Just VUnit),
                            (xs, Just $ Adt "Nil" []),
                            (ys, Just $ Adt "Cons" [VNum 5, Adt "Nil" []]),
                            (len, Nothing),
                            (xy, Just $ VTuple [VNum 0, VNum 1]),
                            (zs, Just $ Adt "Cons" [VNum 5, Adt "Cons" [VNum 4, Adt "Cons" [VNum 3, Adt "Nil" []]]]),
                            (z, Just $ VNum 3)]
        it "should get value of ADT and pattern match expressions part2" $ do
          tvarB <- makeVariable
          let name2 = "Tree"
          let vars2 = [tvarB]
          let dataType2 = TOper name2 vars2
          let nullConstructor = TypeConstructor "Null" []
          let leafConstructor = TypeConstructor "Leaf" [tvarB]
          let nodeConstructor = TypeConstructor "Node" [dataType2, tvarB, dataType2]
          let treeData = EDataDecl name2 dataType2 vars2 [nullConstructor, leafConstructor, nodeConstructor]
          let t = EDestructLetBinding (IdPattern "t") [] [EApp (EApp (EApp (EVar "Node") $ EApp (EVar "Leaf") $ ENum 5) $ ENum 4) $ EApp (EVar "Leaf") $ ENum 3]
          runEvalSpecCases [(treeData, Just VUnit),
                            (t, Just $ Adt "Node" [Adt "Leaf" [VNum 5], VNum 4, Adt "Leaf" [VNum 3]])]
        it "should get value of ADT and pattern match expressions part3" $ do
          let name3 = "Ast"
          let dataType3 = TOper name3 []
          let numConstructor = TypeConstructor "Num" [intT]
          let addConstructor = TypeConstructor "Add" [dataType3, dataType3]
          let subConstructor = TypeConstructor "Sub" [dataType3, dataType3]
          let mulConstructor = TypeConstructor "Mul" [dataType3, dataType3]
          let divConstructor = TypeConstructor "Div" [dataType3, dataType3]
          let astData = EDataDecl name3 dataType3 [] [numConstructor, addConstructor, subConstructor, mulConstructor, divConstructor]
          let evalfn = EDestructLetBinding (IdPattern "eval") [IdPattern "n"] [EPatternMatching (EVar "n") [Case (TConPattern "Num" [IdPattern "a"]) [EVar "a"],Case (TConPattern "Add" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "+") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"],Case (TConPattern "Sub" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "-") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"],Case (TConPattern "Mul" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "*") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"],Case (TConPattern "Div" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "/") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"]]]
          let sym = EDestructLetBinding (IdPattern "sym") [] [EApp (EApp (EVar "Mul") (EApp (EApp (EVar "Add") $ EApp (EVar "Num") $ ENum 4) $ EApp (EVar "Num") $ ENum 3)) (EApp (EApp (EVar "Sub") $ EApp (EVar "Num") $ ENum 4) $ EApp (EVar "Num") $ ENum 1)]
          let result = EDestructLetBinding (IdPattern "result") [] [EApp (EVar "eval") $ EVar "sym"]
          runEvalSpecCases [(astData, Just VUnit),
                            (evalfn, Nothing),
                            (sym, Just $ Adt "Mul" [Adt "Add" [Adt "Num" [VNum 4], Adt "Num" [VNum 3]], Adt "Sub" [Adt "Num" [VNum 4], Adt "Num" [VNum 1]]]),
                            (result, Just $ VNum 21)]
        it "should get value of ADT and pattern match expressions part4" $ do
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
          let result1 = EDestructLetBinding (IdPattern "result1") [] [EApp (EVar "eval1") $ EVar "a"]
          let result2 = EDestructLetBinding (IdPattern "result2") [] [EApp (EVar "eval2") $ EVar "a"]
          let simplify = EDestructLetBinding (IdPattern "simplify") [IdPattern "e"] [EPatternMatching (EVar "e") [Case (TConPattern "App" [TConPattern "Add" [], TConPattern "Num" [IdPattern "n"], IdPattern "e2"]) [EIf (EApp (EApp (EVar "=") $ EVar "n") $ ENum 0) [EVar "e2"] [EVar "e"]]]]
          let b = EDestructLetBinding (IdPattern "b") [] [EApp (EApp (EApp (EVar "App") $ EVar "Add") $ EApp (EVar "Num") $ ENum 0) $ EApp (EVar "Num") $ ENum 6]
          let c = EDestructLetBinding (IdPattern "c") [] [EApp (EVar "simplify") $ EVar "b"]
          runEvalSpecCases [(operData, Just VUnit),
                            (exprData, Just VUnit),
                            (a, Just $ Adt "App" [Adt "Add" [], Adt "Num" [VNum 5], Adt "Num" [VNum 6]]),
                            (eval1, Nothing),
                            (eval2, Nothing),
                            (result1, Just $ VNum 11),
                            (result2, Just $ VNum 11),
                            (simplify, Nothing),
                            (b, Just $ Adt "App" [Adt "Add"[], Adt "Num" [VNum 0], Adt "Num" [VNum 6]]),
                            (c, Just $ Adt "Num" [VNum 6])]
        it "should get value of lambda expressions even with type annotations" $ do
          let g = EDestructLetBinding (IdPattern "g") [] [ELambda [Named "x" Nothing, Named "y" Nothing] Nothing [EApp (EApp (EVar "+") $ EVar "x") $ EVar "y"]]
          let res0 = EDestructLetBinding (IdPattern "res0") [] [EApp (EApp (EVar "g") $ ENum 3) $ ENum 3]
          let f = EDestructLetBinding (IdPattern "f") [] [ELambda [Named "x" (Just intT), Named "y" (Just intT), Named "z" (Just intT)] (Just intT) [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "x") $ EVar "y")) $ EVar "z"]]
          let res1 = EDestructLetBinding (IdPattern "res1") [] [EApp (EApp (EApp (EVar "f") $ ENum 8) $ ENum 2) $ ENum 3]
          let id = EDestructLetBinding (IdPattern "id") [] [ELambda [Named "x" Nothing] Nothing [EVar "x"]]
          let res2 = EDestructLetBinding (IdPattern "res2") [] [EApp (EVar "id") $ ENum 3]
          let res3 = EDestructLetBinding (IdPattern "res3") [] [EApp (EVar "id") $ EBool True]
          let idpair = ELetBinding (IdPattern "id") (ELambda [Named "x" Nothing] Nothing [EVar "x"]) [(ETuple [EApp (EVar "id") (ENum 3), EApp (EVar "id") (EBool True)])]
          runEvalSpecCases [(g, Nothing),
                            (res0, Just $ VNum 6),
                            (f, Nothing),
                            (res1, Just $ VNum 13),
                            (id, Nothing),
                            (res2, Just $ VNum 3),
                            (res3, Just $ VBool True),
                            (idpair, Just $ VTuple [VNum 3, VBool True])]
        it "should get value of function definition, application and pattern match" $ do
          let fib = EDestructLetBinding (IdPattern "fib") [IdPattern "x"] [EPatternMatching (EVar "x") [Case (NumPattern 0) [ENum 0], Case (NumPattern 1) [ENum 1], Case WildcardPattern [EApp (EApp (EVar "+") (EApp (EVar "fib") $ EApp (EApp (EVar "-") $ EVar "x") $ ENum 1)) $ EApp (EVar "fib") $ EApp (EApp (EVar "-") $ EVar "x") $ ENum 2]]]
          let fib0 = EApp (EVar "fib") $ ENum 0
          let fib1 = EApp (EVar "fib") $ ENum 1
          let fib5 = EApp (EVar "fib") $ ENum 5
          let fib6 = EApp (EVar "fib") $ ENum 6
          let penultimate = EProgram [EDestructLetBinding (IdPattern "penultimate") [IdPattern "xs"] [EPatternMatching (EVar "xs") [Case (TConPattern "Nil" []) [ENum 0],
                                                                                                                                    Case (TConPattern "Cons" [WildcardPattern, TConPattern "Nil" []]) [ENum 0],
                                                                                                                                    Case (TConPattern "Cons" [IdPattern "a", TConPattern "Cons" [WildcardPattern, TConPattern "Nil" []]]) [EVar "a"],
                                                                                                                                    Case (TConPattern "Cons" [IdPattern "x", TConPattern "Cons" [IdPattern "y", IdPattern "t"]]) [EApp (EVar "penultimate") (EVar "t")]]]]
          let res7 = EDestructLetBinding (IdPattern "res7") [] [EApp (EVar "penultimate") (EList [ENum 1, ENum 2, ENum 3])]
          let res8 = EDestructLetBinding (IdPattern "res7") [] [EApp (EVar "penultimate") (EList [ENum 1, ENum 2, ENum 3, ENum 4])]
          runEvalSpecCases [(fib, Nothing),
                            (fib0, Just $ VNum 0),
                            (fib1, Just $ VNum 1),
                            (fib5, Just $ VNum 5),
                            (fib6, Just $ VNum 8),
                            (penultimate, Nothing),
                            (res7, Just $ VNum 0),
                            (res8, Just $ VNum 3)]
        it "should get value of basic syntax element" $ do
          let xb = EDestructLetBinding (IdPattern "x") [] [EBool True]
          let d = EDestructLetBinding (IdPattern "d") [] [ETuple [ETuple [ENum 4, EBool True], ETuple [EStr "test", EChar 'c', ENum 45]]]
          runEvalSpecCases [(xb, Just $ VBool True),
                            (d, Just $ VTuple [VTuple [VNum 4, VBool True], VTuple [cons (VChar 't') (cons (VChar 'e') (cons (VChar 's') (cons (VChar 't') nil))), VChar 'c', VNum 45]])]


