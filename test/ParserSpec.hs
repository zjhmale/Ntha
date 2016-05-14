module ParserSpec where

import Ast
import Type
import State
import Parser
import qualified Data.Map as M
import qualified Text.PrettyPrint as PP
import Test.Hspec

spec :: Spec
spec = do
  describe "parser test" $ do
    it "should parse ADT and pattern match expressions part1" $ do
      tvarA <- makeVariable
      let name = "List"
      let vars = [tvarA]
      let dataType = TOper name vars
      let consConstructor = TypeConstructor "Cons" [tvarA, dataType]
      let nilConstructor = TypeConstructor "Nil" []
      let listData = EDataDecl name dataType vars [consConstructor, nilConstructor]
      ((PP.text . show) (parseExpr "(data List a (Cons a (List a)) Nil)")) `shouldBe` ((PP.text . show) (EProgram [listData]))
      parseExpr "(let xs Nil)" `shouldBe` EProgram [EDestructLetBinding (IdPattern "xs") [] [(EVar "Nil")]]
      parseExpr "(let ys (Cons 5 Nil))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "ys") [] [EApp (EApp (EVar "Cons") $ ENum 5) $ EVar "Nil"]]
      parseExpr "(ƒ len [l] (match l (Nil ⇒ 0) ((Cons h t) ⇒ (+ 1 (len t)))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "len") [IdPattern "l"] [EPatternMatching (EVar "l") [Case (TConPattern "Nil" []) [ENum 0],
                                                                                                                                                                                         Case (TConPattern "Cons" [IdPattern "h", IdPattern "t"]) [EApp (EApp (EVar "+") $ ENum 1) $ EApp (EVar "len") $ EVar "t"]]]]
      parseExpr "(let xy ((len xs) . (len ys)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "xy") [] [ETuple [EApp (EVar "len") (EVar "xs"), EApp (EVar "len") (EVar"ys")]]]
      parseExpr "(let zs (Cons 5 (Cons 4 (Cons 3 Nil))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "zs") [] [EApp (EApp (EVar "Cons") $ ENum 5) $ EApp (EApp (EVar "Cons") $ ENum 4) $ EApp (EApp (EVar "Cons") $ ENum 3) $ EVar "Nil"]]
      parseExpr "(let z (len zs))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "z") [] [EApp (EVar "len") $ EVar "zs"]]
    it "should parse ADT and pattern match expressions part2" $ do
      tvarB <- makeVariable
      let name2 = "Tree"
      let vars2 = [tvarB]
      let dataType2 = TOper name2 vars2
      let nullConstructor = TypeConstructor "Null" []
      let leafConstructor = TypeConstructor "Leaf" [tvarB]
      let nodeConstructor = TypeConstructor "Node" [dataType2, tvarB, dataType2]
      let treeData = EDataDecl name2 dataType2 vars2 [nullConstructor, leafConstructor, nodeConstructor]
      ((PP.text . show) (parseExpr "(data Tree a Null (Leaf a) (Node (Tree a) a (Tree a)))")) `shouldBe` ((PP.text . show) (EProgram [treeData]))
      parseExpr "(let t (Node (Leaf 5) 4 (Leaf 3)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "t") [] [EApp (EApp (EApp (EVar "Node") $ EApp (EVar "Leaf") $ ENum 5) $ ENum 4) $ EApp (EVar "Leaf") $ ENum 3]]
    it "should parse ADT and pattern match expressions part3" $ do
      let name3 = "Ast"
      let dataType3 = TOper name3 []
      let numConstructor = TypeConstructor "Num" [intT]
      let addConstructor = TypeConstructor "Add" [dataType3, dataType3]
      let subConstructor = TypeConstructor "Sub" [dataType3, dataType3]
      let mulConstructor = TypeConstructor "Mul" [dataType3, dataType3]
      let divConstructor = TypeConstructor "Div" [dataType3, dataType3]
      let astData = EDataDecl name3 dataType3 [] [numConstructor, addConstructor, subConstructor, mulConstructor, divConstructor]
      parseExpr "(data Ast (Num Number) (Add Ast Ast) (Sub Ast Ast) (Mul Ast Ast) (Div Ast Ast))" `shouldBe` EProgram [astData]
      parseExpr "(ƒ eval [n] (match n ((Num a) => a) ((Add a b) => (+ (eval a) (eval b))) ((Sub a b) => (- (eval a) (eval b))) ((Mul a b) => (* (eval a) (eval b))) ((Div a b) => (/ (eval a) (eval b)))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "eval") [IdPattern "n"] [EPatternMatching (EVar "n") [Case (TConPattern "Num" [IdPattern "a"]) [EVar "a"],
                                                                                                                                                                                                                                                                                                                    Case (TConPattern "Add" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "+") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"],
                                                                                                                                                                                                                                                                                                                    Case (TConPattern "Sub" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "-") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"],
                                                                                                                                                                                                                                                                                                                    Case (TConPattern "Mul" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "*") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"],
                                                                                                                                                                                                                                                                                                                    Case (TConPattern "Div" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "/") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"]]]]
      parseExpr "(let sym (Mul (Add (Num 4) (Num 3)) (Sub (Num 4) (Num 1))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "sym") [] [EApp (EApp (EVar "Mul") (EApp (EApp (EVar "Add") $ EApp (EVar "Num") $ ENum 4) $ EApp (EVar "Num") $ ENum 3)) (EApp (EApp (EVar "Sub") $ EApp (EVar "Num") $ ENum 4) $ EApp (EVar "Num") $ ENum 1)]]
      parseExpr "(let result (eval sym))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "result") [] [EApp (EVar "eval") $ EVar "sym"]]
    it "should parse ADT and pattern match expressions part4" $ do
      let name4 = "Oper"
      let dataType4 = TOper name4 []
      let addOperConstructor = TypeConstructor "Add" []
      let subOperConstructor = TypeConstructor "Sub" []
      let operData = EDataDecl name4 dataType4 [] [addOperConstructor, subOperConstructor]
      parseExpr "(data Oper Add Sub)" `shouldBe` EProgram [operData]
      let name5 = "Expr"
      let dataType5 = TOper name5 []
      let numExprConstructor = TypeConstructor "Num" [intT]
      let appExprConstructor = TypeConstructor "App" [dataType4, dataType5, dataType5]
      let exprData = EDataDecl name5 dataType5 [] [numExprConstructor, appExprConstructor]
      parseExpr "(data Expr (Num Number) (App Oper Expr Expr))" `shouldBe` EProgram [exprData]
      parseExpr "(let a (App Add (Num 5) (Num 6)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "a") [] [EApp (EApp (EApp (EVar "App") $ EVar "Add") $ EApp (EVar "Num") $ ENum 5) $ EApp (EVar "Num") $ ENum 6]]
      parseExpr "(ƒ eval [e] (match e ((Num n) => n) ((App o e1 e2) => (match o (Add => (+ (eval e1) (eval e2))) (Sub => (- (eval e1) (eval e2)))))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "eval") [IdPattern "e"] [EPatternMatching (EVar "e") [Case (TConPattern "Num" [IdPattern "n"]) [EVar "n"],
                                                                                                                                                                                                                                                               Case (TConPattern "App" [IdPattern "o", IdPattern "e1", IdPattern "e2"]) [EPatternMatching (EVar "o") [Case (TConPattern "Add" []) [EApp (EApp (EVar "+") $ EApp (EVar "eval") $ EVar "e1") $ EApp (EVar "eval") $ EVar "e2"],
                                                                                                                                                                                                                                                                                                                                                                      Case (TConPattern "Sub" []) [EApp (EApp (EVar "-") $ EApp (EVar "eval") $ EVar "e1") $ EApp (EVar "eval") $ EVar "e2"]]]]]]
      parseExpr "(ƒ eval [e] (match e ((Num n) => n) ((App Add e1 e2) => (+ (eval e1) (eval e2))) ((App Sub e1 e2) => (- (eval e1) (eval e2)))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "eval") [IdPattern "e"] [EPatternMatching (EVar "e") [Case (TConPattern "Num" [IdPattern "n"]) [EVar "n"],
                                                                                                                                                                                                                                                           Case (TConPattern "App" [TConPattern "Add" [], IdPattern "e1", IdPattern "e2"]) [EApp (EApp (EVar "+") $ EApp (EVar "eval") $ EVar "e1") $ EApp (EVar "eval") $ EVar "e2"],
                                                                                                                                                                                                                                                           Case (TConPattern "App" [TConPattern "Sub" [], IdPattern "e1", IdPattern "e2"]) [EApp (EApp (EVar "-") $ EApp (EVar "eval") $ EVar "e1") $ EApp (EVar "eval") $ EVar "e2"]]]]
      parseExpr "(let av (eval a))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "av") [] [EApp (EVar "eval") $ EVar "a"]]
      parseExpr "(ƒ simplify [e] (match e ((App Add (Num n) e2) => (if (= n 0) e2 e))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "simplify") [IdPattern "e"] [EPatternMatching (EVar "e") [Case (TConPattern "App" [TConPattern "Add" [], TConPattern "Num" [IdPattern "n"], IdPattern "e2"]) [EIf (EApp (EApp (EVar "=") $ EVar "n") $ ENum 0) [EVar "e2"] [EVar "e"]]]]]
      parseExpr "(let a (App Add (Num 0) (Num 6)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "a") [] [EApp (EApp (EApp (EVar "App") $ EVar "Add") $ EApp (EVar "Num") $ ENum 0) $ EApp (EVar "Num") $ ENum 6]]
      parseExpr "(let b (simplify a))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "b") [] [EApp (EVar "simplify") $ EVar "a"]]
    it "should parse lambda expressions even with type annotations" $ do
      parseExpr "(let g (λx y ⇒ (+ x y)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "g") [] [ELambda [Named "x" Nothing, Named "y" Nothing] Nothing [EApp (EApp (EVar "+") $ EVar "x") $ EVar "y"]]]
      parseExpr "(let res0 (g 3 3))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "res0") [] [EApp (EApp (EVar "g") $ ENum 3) $ ENum 3]]
      parseExpr "(let id (λx ⇒ x))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "id") [] [ELambda [Named "x" Nothing] Nothing [EVar "x"]]]
      parseExpr "(let res2 (id 3))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "res2") [] [EApp (EVar "id") $ ENum 3]]
      parseExpr "(let res3 (id true))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "res3") [] [EApp (EVar "id") $ EBool True]]
      parseExpr "(let [id (λx ⇒ x)] ((id 3) . (id true)))" `shouldBe` EProgram [ELetBinding (IdPattern "id") (ELambda [Named "x" Nothing] Nothing [EVar "x"]) [(ETuple [EApp (EVar "id") (ENum 3), EApp (EVar "id") (EBool True)])]]
      parseExpr "(let [id (λx ⇒ x) a 3 b (+ a 3)] ((id a) . (id b)))" `shouldBe` EProgram [ELetBinding (IdPattern "id") (ELambda [Named "x" Nothing] Nothing [EVar "x"]) [ELetBinding (IdPattern "a") (ENum 3) [ELetBinding (IdPattern "b") (EApp (EApp (EVar "+") $ EVar "a") $ ENum 3) [(ETuple [EApp (EVar "id") (EVar "a"), EApp (EVar "id") (EVar "b")])]]]]
      parseExpr "(let f (λ(x: Number) (y: Number) (z: Number) : Number => (+ x y z)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "f") [] [ELambda [Named "x" (Just intT), Named "y" (Just intT), Named "z" (Just intT)] (Just intT) [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "x") $ EVar "y")) $ EVar "z"]]]
      parseExpr "(let f (λx y z => (+ x y z)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "f") [] [ELambda [Named "x" Nothing, Named "y" Nothing, Named "z" Nothing] Nothing [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ EVar "x") $ EVar "y")) $ EVar "z"]]]
      parseExpr "(let res (f 8 2 3))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "res") [] [EApp (EApp (EApp (EVar "f") $ ENum 8) $ ENum 2) $ ENum 3]]
    it "should parse function definition, application and pattern match" $ do
      parseExpr "(ƒ fib [x]\n (match x\n (0 => 0)\n (1 => 1)\n (_ => (+ (fib (- x 1)) (fib (- x 2))))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "fib") [IdPattern "x"] [EPatternMatching (EVar "x") [Case (NumPattern 0) [ENum 0],
                                                                                                                                                                                                                 Case (NumPattern 1) [ENum 1],
                                                                                                                                                                                                                 Case WildcardPattern [EApp (EApp (EVar "+") (EApp (EVar "fib") $ EApp (EApp (EVar "-") $ EVar "x") $ ENum 1)) $ EApp (EVar "fib") $ EApp (EApp (EVar "-") $ EVar "x") $ ENum 2]]]]
      parseExpr "(ƒ penultimate [xs]\n (match xs\n ([] => 0)\n ([_] => 0)\n ([a _] => a)\n (x :: y :: t => (penultimate t))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "penultimate") [IdPattern "xs"] [EPatternMatching (EVar "xs") [Case (TConPattern "Nil" []) [ENum 0],
                                                                                                                                                                                                                                              Case (TConPattern "Cons" [WildcardPattern, TConPattern "Nil" []]) [ENum 0],
                                                                                                                                                                                                                                              Case (TConPattern "Cons" [IdPattern "a", TConPattern "Cons" [WildcardPattern, TConPattern "Nil" []]]) [EVar "a"],
                                                                                                                                                                                                                                              Case (TConPattern "Cons" [IdPattern "x", TConPattern "Cons" [IdPattern "y", IdPattern "t"]]) [EApp (EVar "penultimate") (EVar "t")]]]]
      parseExpr "(let res4 (penultimate [1 2 3]))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "res4") [] [EApp (EVar "penultimate") (EList [ENum 1, ENum 2, ENum 3])]]
      parseExpr "(let x (penultimate [[\"g\"] [\"c\"]]))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "x") [] [EApp (EVar "penultimate") (EList [EList [EStr "g"], EList [EStr "c"]])]]
      parseExpr "(ƒ map [f l] (match l ((Cons h t) => (Cons (f h) (map f t))) (Nil => Nil)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "map") [IdPattern "f", IdPattern "l"] [EPatternMatching (EVar "l") [Case (TConPattern "Cons" [IdPattern "h", IdPattern "t"]) [EApp (EApp (EVar "Cons") $ EApp (EVar "f") $ EVar "h") $ EApp (EApp (EVar "map") $ EVar "f") $ EVar "t"],
                                                                                                                                                                                                                     Case (TConPattern "Nil" []) [EVar "Nil"]]]]
      parseExpr "(ƒ map [f xs] (match xs ([] ⇒ []) (h :: t ⇒ ((f h) :: (map f t)))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "map") [IdPattern "f", IdPattern "xs"] [EPatternMatching (EVar "xs") [Case (TConPattern "Nil" []) [EList []],
                                                                                                                                                                                                               Case (TConPattern "Cons" [IdPattern "h", IdPattern "t"]) [EApp (EApp (EVar "Cons") $ EApp (EVar "f") $ EVar "h") $ EApp (EApp (EVar "map") $ EVar "f") $ EVar "t"]]]]
      parseExpr "(let l3 (map (λx => (= (% x 2) 0)) l))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "l3") [] [EApp (EApp (EVar "map") $ ELambda [Named "x" Nothing] Nothing [EApp (EApp (EVar "=") $ EApp (EApp (EVar "%") $ EVar "x") $ ENum 2) $ ENum 0]) $ EVar "l"]]
      parseExpr "(ƒ k [x y] (match (x . y) ((0 . 0) => 0) (_ => 1)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "k") [IdPattern "x", IdPattern "y"] [EPatternMatching (ETuple [EVar "x", EVar "y"]) [Case (TuplePattern [NumPattern 0, NumPattern 0]) [ENum 0], Case WildcardPattern [ENum 1]]]]
      parseExpr "(ƒ fact [n] (if (≤ n 1) 1 (* n (fact (- n 1)))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "fact") [IdPattern "n"] [EIf (EApp (EApp (EVar "≤") $ EVar "n") $ ENum 1) [ENum 1] [EApp (EApp (EVar "*") $ EVar "n") (EApp (EVar "fact") $ EApp (EApp (EVar "-") $ EVar "n") $ ENum 1)]]]
      parseExpr "(let f5 (fact 5))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "f5") [] [EApp (EVar "fact") $ ENum 5]]
      parseExpr "(ƒ comp [f g x] (f (g x)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "comp") [IdPattern "f", IdPattern "g", IdPattern "x"] [EApp (EVar "f") (EApp (EVar "g") (EVar "x"))]]
      parseExpr "(let fix (comp inc dec))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "fix") [] [EApp (EApp (EVar "comp") $ EVar "inc") (EVar "dec")]]
      parseExpr "(let incdec (fix 5))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "incdec") [] [EApp (EVar "fix") (ENum 5)]]
      parseExpr "(ƒ len2 [xs] (match xs ([] => 0) (_ :: t => (+ 1 (len2 t)))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "len2") [IdPattern "xs"] [EPatternMatching (EVar "xs") [Case (TConPattern "Nil" []) [ENum 0],
                                                                                                                                                                                          Case (TConPattern "Cons" [WildcardPattern, IdPattern "t"]) [EApp (EApp (EVar "+") $ ENum 1) (EApp (EVar "len2") $ EVar "t")]]]]
      parseExpr "(len2 y)" `shouldBe` EProgram [EApp (EVar "len2") $ EVar "y"]
      parseExpr "(ƒ append [x xs] (x :: xs))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "append") [IdPattern "x", IdPattern "xs"] [EApp (EApp (EVar "Cons") $ EVar "x") $ EVar "xs"]]
      parseExpr "(let l2 (append 0 l))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "l2") [] [EApp (EApp (EVar "append") $ ENum 0) $ EVar "l"]]
      parseExpr "(let patmat0 (match (\"a\" . 3) (a => (\"ok\" . a))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "patmat0") [] [EPatternMatching (ETuple [EStr "a", ENum 3]) [Case (IdPattern "a") [ETuple [EStr "ok", EVar "a"]]]]]
      parseExpr "(let patmat1 (match (\"a\" . 3) ((a . b) => (\"ok\" . a . b))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "patmat1") [] [EPatternMatching (ETuple [EStr "a", ENum 3]) [Case (TuplePattern [IdPattern "a", IdPattern "b"]) [ETuple [EStr "ok", EVar "a", EVar "b"]]]]]
      parseExpr "(let patmat2 (match (\"a\" . 3) ((a . _) => (\"ok\" . a))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "patmat2") [] [EPatternMatching (ETuple [EStr "a", ENum 3]) [Case (TuplePattern [IdPattern "a", WildcardPattern]) [ETuple [EStr "ok", EVar "a"]]]]]
      parseExpr "(let patmat3 (match 'a' ('a' => true) (_ => false)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "patmat3") [] [EPatternMatching (EChar 'a') [Case (CharPattern 'a') [EBool True], Case WildcardPattern [EBool False]]]]
      parseExpr "(let patmat4 (match true (true => true) (_ => false)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "patmat4") [] [EPatternMatching (EBool True) [Case (BoolPattern True) [EBool True], Case WildcardPattern [EBool False]]]]
      parseExpr "(let patmat5 (match 1 (1 => true) (_ => false)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "patmat5") [] [EPatternMatching (ENum 1) [Case (NumPattern 1) [EBool True], Case WildcardPattern [EBool False]]]]
      parseExpr "(let patmat6 (match \"abc\" (\"abc\" => true) (_ => false)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "patmat6") [] [EPatternMatching (EStr "abc") [Case (TConPattern "Cons" [CharPattern 'a', (TConPattern "Cons" [CharPattern 'b', (TConPattern "Cons" [CharPattern 'c', TConPattern "Nil" []])])]) [EBool True], Case WildcardPattern [EBool False]]]]
    it "should parse basic syntax element" $ do
      parseExpr "(let x true)" `shouldBe` EProgram [EDestructLetBinding (IdPattern "x") [] [EBool True]]
      parseExpr "(let d ((4 . true) . (\"test\" . 'c' . 45)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "d") [] [ETuple [ETuple [ENum 4, EBool True], ETuple [EStr "test", EChar 'c', ENum 45]]]]
      parseExpr "(+ 1 2 3 4 5)" `shouldBe` EProgram [EApp (EApp (EVar "+") (EApp (EApp (EVar "+") (EApp (EApp (EVar "+") (EApp (EApp (EVar "+") $ ENum 1) $ ENum 2)) $ ENum 3)) $ ENum 4)) $ ENum 5]
      parseExpr "(let y [1 2 3])" `shouldBe` EProgram [EDestructLetBinding (IdPattern "y") [] [EList [ENum 1, ENum 2, ENum 3]]]
      parseExpr "(let z [])" `shouldBe` EProgram [EDestructLetBinding (IdPattern "z") [] [EList []]]
      parseExpr "(let a 'a')" `shouldBe` EProgram [EDestructLetBinding (IdPattern "a") [] [EChar 'a']]
      parseExpr "(let s \"qdsfsdf\")" `shouldBe` EProgram [EDestructLetBinding (IdPattern "s") [] [EStr "qdsfsdf"]]
      parseExpr "(let l (1 :: 2 :: 3 :: Nil))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "l") [] [EApp (EApp (EVar "Cons") $ ENum 1) $ EApp (EApp (EVar "Cons") $ ENum 2) $ EApp (EApp (EVar "Cons") $ ENum 3) $ EVar "Nil"]]
      parseExpr "(let profile {:name \"ntha\" :age 12})" `shouldBe` EProgram [EDestructLetBinding (IdPattern "profile") [] [ERecord (M.fromList [("name", EStr "ntha"), ("age", ENum 12)])]]
      parseExpr "(:name profile)" `shouldBe` EProgram [EAccessor (EVar "profile") "name"]
