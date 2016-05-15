{-# LANGUAGE UnicodeSyntax #-}

module EvalSpec where

import Ast
import Type
import Value
import Eval
import Prologue
import qualified Data.Map as M
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
          let idpair2 = ELetBinding (IdPattern "id") (ELambda [Named "x" Nothing] Nothing [EVar "x"]) [ELetBinding (IdPattern "a") (ENum 3) [ELetBinding (IdPattern "b") (EApp (EApp (EVar "+") $ EVar "a") $ ENum 3) [(ETuple [EApp (EVar "id") (EVar "a"), EApp (EVar "id") (EVar "b")])]]]
          let f1 = EDestructLetBinding (IdPattern "f1") [] [ELambda [Named "x" (Just intT), Named "y" (Just intT), Named "z" (Just intT)] (Just intT) [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "x") $ EVar "y")) $ EVar "z"]]
          let f2 = EDestructLetBinding (IdPattern "f2") [] [ELambda [Named "x" Nothing, Named "y" Nothing, Named "z" Nothing] Nothing [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "x") $ EVar "y")) $ EVar "z"]]
          let f1res = EDestructLetBinding (IdPattern "f1res") [] [EApp (EApp (EApp (EVar "f1") $ ENum 8) $ ENum 2) $ ENum 3]
          let f2res = EDestructLetBinding (IdPattern "f2res") [] [EApp (EApp (EApp (EVar "f2") $ ENum 8) $ ENum 2) $ ENum 3]
          runEvalSpecCases [(g, Nothing),
                            (res0, Just $ VNum 6),
                            (f, Nothing),
                            (res1, Just $ VNum 13),
                            (id, Nothing),
                            (res2, Just $ VNum 3),
                            (res3, Just $ VBool True),
                            (idpair, Just $ VTuple [VNum 3, VBool True]),
                            (idpair2, Just $ VTuple [VNum 3, VNum 6]),
                            (f1, Nothing),
                            (f2, Nothing),
                            (f1res, Just $ VNum 13),
                            (f2res, Just $ VNum 13)]
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
          let map = EDestructLetBinding (IdPattern "map") [IdPattern "f", IdPattern "l"] [EPatternMatching (EVar "l") [Case (TConPattern "Cons" [IdPattern "h", IdPattern "t"]) [EApp (EApp (EVar "Cons") $ EApp (EVar "f") $ EVar "h") $ EApp (EApp (EVar "map") $ EVar "f") $ EVar "t"],Case (TConPattern "Nil" []) [EVar "Nil"]]]
          let map2 = EDestructLetBinding (IdPattern "map2") [IdPattern "f", IdPattern "xs"] [EPatternMatching (EVar "xs") [Case (TConPattern "Nil" []) [EList []],Case (TConPattern "Cons" [IdPattern "h", IdPattern "t"]) [EApp (EApp (EVar "Cons") $ EApp (EVar "f") $ EVar "h") $ EApp (EApp (EVar "map2") $ EVar "f") $ EVar "t"]]]
          let l = EDestructLetBinding (IdPattern "l") [] [EList [ENum 1, ENum 2, ENum 3]]
          let l3 = EDestructLetBinding (IdPattern "l3") [] [EApp (EApp (EVar "map") $ ELambda [Named "x" Nothing] Nothing [EApp (EApp (EVar "=") $ EApp (EApp (EVar "%") $ EVar "x") $ ENum 2) $ ENum 0]) $ EVar "l"]
          let l6 = EDestructLetBinding (IdPattern "l6") [] [EApp (EApp (EVar "map2") $ ELambda [Named "x" Nothing] Nothing [EApp (EApp (EVar "=") $ EApp (EApp (EVar "%") $ EVar "x") $ ENum 2) $ ENum 0]) $ EVar "l"]
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
          let patmat3 = EDestructLetBinding (IdPattern "patmat3") [] [EPatternMatching (EChar 'a') [Case (CharPattern 'a') [EBool True], Case WildcardPattern [EBool False]]]
          let patmat4 = EDestructLetBinding (IdPattern "patmat4") [] [EPatternMatching (EBool True) [Case (BoolPattern True) [EBool True], Case WildcardPattern [EBool False]]]
          let patmat5 = EDestructLetBinding (IdPattern "patmat5") [] [EPatternMatching (ENum 1) [Case (NumPattern 1) [EBool True], Case WildcardPattern [EBool False]]]
          let patmat6 = EDestructLetBinding (IdPattern "patmat6") [] [EPatternMatching (EStr "abc") [Case (TConPattern "Cons" [CharPattern 'a', (TConPattern "Cons" [CharPattern 'b', (TConPattern "Cons" [CharPattern 'c', TConPattern "Nil" []])])]) [EBool True], Case WildcardPattern [EBool False]]]
          let patmat7 = EDestructLetBinding (IdPattern "patmat7") [] [EPatternMatching (EStr "acb") [Case (TConPattern "Cons" [CharPattern 'a', (TConPattern "Cons" [CharPattern 'b', (TConPattern "Cons" [CharPattern 'c', TConPattern "Nil" []])])]) [EBool True], Case WildcardPattern [EBool False]]]
          runEvalSpecCases [(fib, Nothing),
                            (fib0, Just $ VNum 0),
                            (fib1, Just $ VNum 1),
                            (fib5, Just $ VNum 5),
                            (fib6, Just $ VNum 8),
                            (penultimate, Nothing),
                            (res7, Just $ VNum 0),
                            (res8, Just $ VNum 3),
                            (map, Nothing),
                            (map2, Nothing),
                            (l, Just $ cons (VNum 1) (cons (VNum 2) (cons (VNum 3) nil))),
                            (l3, Just $ cons (VBool False) (cons (VBool True) (cons (VBool False) nil))),
                            (l6, Just $ cons (VBool False) (cons (VBool True) (cons (VBool False) nil))),
                            (k, Nothing),
                            (fact, Nothing),
                            (f5, Just $ VNum 120),
                            (comp, Nothing),
                            (fix, Nothing),
                            (incdec, Just $ VNum 5),
                            (len, Nothing),
                            (lenl, Just $ VNum 3),
                            (append, Nothing),
                            (l2, Just $ cons (VNum 0) (cons (VNum 1) (cons (VNum 2) (cons (VNum 3) nil)))),
                            (patmat0, Just $ VTuple [cons (VChar 'o') (cons (VChar 'k') nil), VTuple [cons (VChar 'a') nil, VNum 3]]),
                            (patmat1, Just $ VTuple [cons (VChar 'o') (cons (VChar 'k') nil), cons (VChar 'a') nil, VNum 3]),
                            (patmat2, Just $ VTuple [cons (VChar 'o') (cons (VChar 'k') nil), cons (VChar 'a') nil]),
                            (patmat3, Just $ VBool True),
                            (patmat4, Just $ VBool True),
                            (patmat5, Just $ VBool True),
                            (patmat6, Just $ VBool True),
                            (patmat7, Just $ VBool False)]
        it "should get value of basic syntax element" $ do
          let xb = EDestructLetBinding (IdPattern "x") [] [EBool True]
          let d = EDestructLetBinding (IdPattern "d") [] [ETuple [ETuple [ENum 4, EBool True], ETuple [EStr "test", EChar 'c', ENum 45]]]
          let intsum = EApp (EApp (EVar "+") (EApp (EApp (EVar "+") (EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ ENum 1) $ ENum 2)) $ ENum 3)) $ ENum 4)) $ ENum 5
          let l = EDestructLetBinding (IdPattern "y") [] [EList [ENum 1, ENum 2, ENum 3]]
          let l2 = EDestructLetBinding (IdPattern "z") [] [EList []]
          let a = EDestructLetBinding (IdPattern "a") [] [EChar 'a']
          let s = EDestructLetBinding (IdPattern "s") [] [EStr "str"]
          let l3 = EDestructLetBinding (IdPattern "l") [] [EApp (EApp (EVar "Cons") $ ENum 1) $ EApp (EApp (EVar "Cons") $ ENum 2) $ EApp (EApp (EVar "Cons") $ ENum 3) $ EVar "Nil"]
          let profile = EDestructLetBinding (IdPattern "profile") [] [ERecord (M.fromList [("name", EStr "ntha"), ("age", ENum 12)])]
          let name = EAccessor (EVar "profile") "name"
          let equal = (EApp (EApp (EVar "=") $ ENum 3) $ ENum 3)
          let notequal = (EApp (EApp (EVar "≠") $ EBool True) $ EBool False)
          runEvalSpecCases [(xb, Just $ VBool True),
                            (d, Just $ VTuple [VTuple [VNum 4, VBool True], VTuple [cons (VChar 't') (cons (VChar 'e') (cons (VChar 's') (cons (VChar 't') nil))), VChar 'c', VNum 45]]),
                            (intsum, Just $ VNum 15),
                            (l, Just $ cons (VNum 1) (cons (VNum 2) (cons (VNum 3) nil))),
                            (l2, Just $ nil),
                            (a, Just $ VChar 'a'),
                            (s, Just $ cons (VChar 's') (cons (VChar 't') (cons (VChar 'r') nil))),
                            (l3, Just $ cons (VNum 1) (cons (VNum 2) (cons (VNum 3) nil))),
                            (profile, Just $ VRecord (M.fromList [("name", cons (VChar 'n') (cons (VChar 't') (cons (VChar 'h') (cons (VChar 'a') nil)))), ("age", VNum 12)])),
                            (name, Just $ cons (VChar 'n') (cons (VChar 't') (cons (VChar 'h') (cons (VChar 'a') nil)))),
                            (equal, Just $ VBool True),
                            (notequal, Just $ VBool True)]
        it "should get value of destructuring" $ do
          let abpair = EDestructLetBinding (TuplePattern [IdPattern "a", IdPattern "b"]) [] [ETuple [ENum 3, EStr "d"]]
          let d = EDestructLetBinding (IdPattern "d") [] [ETuple [ETuple [ENum 3, EBool True], ETuple [EStr "test", EChar 'c', EVar "a"]]]
          let bool = EDestructLetBinding (TuplePattern [TuplePattern [WildcardPattern, IdPattern "bool"], TuplePattern [WildcardPattern, WildcardPattern, WildcardPattern]]) [] [EVar "d"]
          let boolv = EVar "bool"
          let abctuple = ELetBinding (TuplePattern [IdPattern "a", IdPattern "b", IdPattern "c"]) (ETuple [ENum 1, ENum 2, ENum 3]) [(EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "a") $ EVar "b")) $ EVar "c")]
          let abclist = EDestructLetBinding (TConPattern "Cons" [IdPattern "a", TConPattern "Cons" [IdPattern "b", TConPattern "Cons" [IdPattern "c", TConPattern "Nil" []]]]) [] [EList [ENum 1, ENum 2, ENum 3]]
          let a = EVar "a"
          let b = EVar "b"
          let c = EVar "c"
          let abclist2 = ELetBinding (TConPattern "Cons" [IdPattern "a", TConPattern "Cons" [IdPattern "b", TConPattern "Cons" [IdPattern "c", TConPattern "Nil" []]]]) (EList [ENum 1, ENum 2, ENum 3]) [(EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "a") $ EVar "b")) $ EVar "c")]
          let abctuplefn = EDestructLetBinding (IdPattern "f1") [(TuplePattern [IdPattern "a", IdPattern "b", IdPattern "c"])] [(EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "a") $ EVar "b")) $ EVar "c")]
          let abclistfn = EDestructLetBinding (IdPattern "f2") [(TConPattern "Cons" [IdPattern "a", TConPattern "Cons" [IdPattern "b", TConPattern "Cons" [IdPattern "c", TConPattern "Nil" []]]])] [(EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "a") $ EVar "b")) $ EVar "c")]
          let res1 = EApp (EVar "f1") $ ETuple [EVar "a", EVar "b", EVar "c"]
          let res2 = EApp (EVar "f2") $ EList [EVar "a", EVar "b", EVar "c"]
          tvarA <- makeVariable
          let name = "Maybe"
          let vars = [tvarA]
          let dataType = TOper name vars
          let justConstructor = TypeConstructor "Just" [tvarA]
          let nothingConstructor = TypeConstructor "Nothing" []
          let maybeData = EDataDecl name dataType vars [justConstructor, nothingConstructor]
          let f = EDestructLetBinding (IdPattern "f3") [(TConPattern "Just" [IdPattern "a"])] [(EApp (EApp (EVar "+") $ EVar "a") $ ENum 1)]
          let res3 = EApp (EVar "f3") $ EApp (EVar "Just") $ ENum 2
          let just = EDestructLetBinding (TConPattern "Just" [IdPattern "k"]) [] [EApp (EVar "Just") $ ENum 3]
          let k = EVar "k"
          runEvalSpecCases [(abpair, Just $ VTuple [VNum 3, makeList [VChar 'd']]),
                            (d, Just $ VTuple [VTuple [VNum 3, VBool True], VTuple [makeList [VChar 't', VChar 'e', VChar 's', VChar 't'], VChar 'c', VNum 3]]),
                            (bool, Just $ VTuple [VTuple [VNum 3, VBool True], VTuple [makeList [VChar 't', VChar 'e', VChar 's', VChar 't'], VChar 'c', VNum 3]]),
                            (boolv, Just $ VBool True),
                            (abctuple, Just $ VNum 6),
                            (abclist, Just $ makeList [VNum 1, VNum 2, VNum 3]),
                            (a, Just $ VNum 1),
                            (b, Just $ VNum 2),
                            (c, Just $ VNum 3),
                            (abclist2, Just $ VNum 6),
                            (abctuplefn, Nothing),
                            (abclistfn, Nothing),
                            (res1, Just $ VNum 6),
                            (res2, Just $ VNum 6),
                            (maybeData, Just VUnit),
                            (f, Nothing),
                            (res3, Just $ VNum 3),
                            (just, Just $ Adt "Just" [VNum 3]),
                            (k, Just $ VNum 3)]