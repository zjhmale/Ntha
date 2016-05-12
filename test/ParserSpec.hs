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
      let consConstructor = TypeConstructor "Cons" [tvarA, dataType]
      let nilConstructor = TypeConstructor "Nil" []
      let listData = EDataDecl name dataType vars [consConstructor, nilConstructor]
      tvarB <- makeVariable
      let name2 = "Tree"
      let vars2 = [tvarB]
      let dataType2 = TOper name2 vars2
      let nullConstructor = TypeConstructor "Null" []
      let leafConstructor = TypeConstructor "Leaf" [tvarB]
      let nodeConstructor = TypeConstructor "Node" [dataType2, tvarB, dataType2]
      let treeData = EDataDecl name2 dataType2 vars2 [nullConstructor, leafConstructor, nodeConstructor]
      ((PP.text . show) (parseExpr "(data List a (Cons a (List a)) Nil)")) `shouldBe` ((PP.text . show) (EProgram [listData]))
      ((PP.text . show) (parseExpr "(data Tree a Null (Leaf a) (Node (Tree a) a (Tree a)))")) `shouldBe` ((PP.text . show) (EProgram [treeData]))
      parseExpr "(let t (Node (Leaf 5) 4 (Leaf 3)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "t") [] [EApp (EApp (EApp (EVar "Node") $ EApp (EVar "Leaf") $ ENum 5) $ ENum 4) $ EApp (EVar "Leaf") $ ENum 3]]
      parseExpr "(let xs Nil)" `shouldBe` EProgram [EDestructLetBinding (IdPattern "xs") [] [(EVar "Nil")]]
      parseExpr "(let ys (Cons 5 Nil))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "ys") [] [EApp (EApp (EVar "Cons") $ ENum 5) $ EVar "Nil"]]
      parseExpr "(ƒ len [l] (match l (Nil ⇒ 0) (Cons h t ⇒ (+ 1 (len t)))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "len") [IdPattern "l"] [EPatternMatching (EVar "l") [Case (TConPattern "Nil" []) [ENum 0], Case (TConPattern "Cons" [IdPattern "h", IdPattern "t"]) [EApp (EApp (EVar "+") $ ENum 1) $ EApp (EVar "len") $ EVar "t"]]]]
      parseExpr "(let xy <(len xs) (len ys)>)" `shouldBe` EProgram [EDestructLetBinding (IdPattern "xy") [] [ETuple [EApp (EVar "len") (EVar "xs"), EApp (EVar "len") (EVar"ys")]]]
      parseExpr "(let zs (Cons 5 (Cons 4 (Cons 3 Nil))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "zs") [] [EApp (EApp (EVar "Cons") $ ENum 5) $ EApp (EApp (EVar "Cons") $ ENum 4) $ EApp (EApp (EVar "Cons") $ ENum 3) $ EVar "Nil"]]
      parseExpr "(let z (len zs))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "z") [] [EApp (EVar "len") $ EVar "zs"]]
      parseExpr "(let g (λx y ⇒ (+ x y)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "g") [] [ELambda [Named "x" Nothing, Named "y" Nothing] Nothing [EApp (EApp (EVar "+") $ EVar "x") $ EVar "y"]]]
      parseExpr "(let res0 (g 3 3))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "res0") [] [EApp (EApp (EVar "g") $ ENum 3) $ ENum 3]]
      parseExpr "(let id (λx ⇒ x))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "id") [] [ELambda [Named "x" Nothing] Nothing [EVar "x"]]]
      parseExpr "(let res2 (id 3))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "res2") [] [EApp (EVar "id") $ ENum 3]]
      parseExpr "(let res3 (id true))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "res3") [] [EApp (EVar "id") $ EBool True]]
      parseExpr "(let [id (λx ⇒ x)] <(id 3) (id true)>)" `shouldBe` EProgram [ELetBinding (IdPattern "id") (ELambda [Named "x" Nothing] Nothing [EVar "x"]) [(ETuple [EApp (EVar "id") (ENum 3), EApp (EVar "id") (EBool True)])]]
      parseExpr "(let x true)" `shouldBe` EProgram [EDestructLetBinding (IdPattern "x") [] [EBool True]]
      parseExpr "(let d <<4 true> <\"test\" 'c' 45>>)" `shouldBe` EProgram [EDestructLetBinding (IdPattern "d") [] [ETuple [ETuple [ENum 4, EBool True], ETuple [EStr "test", EChar 'c', ENum 45]]]]
      parseExpr "(ƒ fib [x]\n (match x\n (0 => 0)\n (1 => 1)\n (_ => (+ (fib (- x 1)) (fib (- x 2))))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "fib") [IdPattern "x"] [EPatternMatching (EVar "x") [Case (NumPattern 0) [ENum 0], Case (NumPattern 1) [ENum 1], Case WildcardPattern [EApp (EApp (EVar "+") (EApp (EVar "fib") $ EApp (EApp (EVar "-") $ EVar "x") $ ENum 1)) $ EApp (EVar "fib") $ EApp (EApp (EVar "-") $ EVar "x") $ ENum 2]]]]
      parseExpr "(ƒ penultimate [xs]\n (match xs\n ([] => 0)\n ([_] => 0)\n ([a _] => a)\n (x :: y :: t => (penultimate t))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "penultimate") [IdPattern "xs"] [EPatternMatching (EVar "xs") [Case (TConPattern "Nil" []) [ENum 0],
                                                                                                                                                                                                                                              Case (TConPattern "Cons" [WildcardPattern, TConPattern "Nil" []]) [ENum 0],
                                                                                                                                                                                                                                              Case (TConPattern "Cons" [IdPattern "a", TConPattern "Cons" [WildcardPattern, TConPattern "Nil" []]]) [EVar "a"],
                                                                                                                                                                                                                                              Case (TConPattern "Cons" [IdPattern "x", TConPattern "Cons" [IdPattern "y", IdPattern "t"]]) [EApp (EVar "penultimate") (EVar "t")]]]]
      parseExpr "(let x (penultimate [[\"g\"] [\"c\"]]))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "x") [] [EApp (EVar "penultimate") (EList [EList [EStr "g"], EList [EStr "c"]])]]
      parseExpr "(+ 1 2 3)" `shouldBe` EProgram [EApp (EApp (EApp (EVar "+") $ ENum 1) $ ENum 2) (ENum 3)]
      parseExpr "(let y [1 2 3])" `shouldBe` EProgram [EDestructLetBinding (IdPattern "y") [] [EList [ENum 1, ENum 2, ENum 3]]]
      parseExpr "(let z [])" `shouldBe` EProgram [EDestructLetBinding (IdPattern "z") [] [EList []]]
      parseExpr "(ƒ comp [f g x] (f (g x)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "comp") [IdPattern "f", IdPattern "g", IdPattern "x"] [EApp (EVar "f") (EApp (EVar "g") (EVar "x"))]]
      parseExpr "(let fix (comp inc dec))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "fix") [] [EApp (EApp (EVar "comp") $ EVar "inc") (EVar "dec")]]
      parseExpr "(let incdec (fix 5))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "incdec") [] [EApp (EVar "fix") (ENum 5)]]
      parseExpr "(let a 'a')" `shouldBe` EProgram [EDestructLetBinding (IdPattern "a") [] [EChar 'a']]
      parseExpr "(let s \"qdsfsdf\")" `shouldBe` EProgram [EDestructLetBinding (IdPattern "s") [] [EStr "qdsfsdf"]]
      parseExpr "(ƒ len2 [xs] (match xs ([] => 0) (_ :: t => (+ 1 (len2 t)))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "len2") [IdPattern "xs"] [EPatternMatching (EVar "xs") [Case (TConPattern "Nil" []) [ENum 0],
                                                                                                                                                                                          Case (TConPattern "Cons" [WildcardPattern, IdPattern "t"]) [EApp (EApp (EVar "+") $ ENum 1) (EApp (EVar "len2") $ EVar "t")]]]]
      parseExpr "(len2 y)" `shouldBe` EProgram [EApp (EVar "len2") $ EVar "y"]
      parseExpr "(ƒ append [x xs] (x :: xs))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "append") [IdPattern "x", IdPattern "xs"] [EApp (EApp (EVar "Cons") $ EVar "x") $ EVar "xs"]]
      parseExpr "(let l (1 :: 2 :: 3 :: Nil))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "l") [] [EApp (EApp (EVar "Cons") $ ENum 1) $ EApp (EApp (EVar "Cons") $ ENum 2) $ EApp (EApp (EVar "Cons") $ ENum 3) $ EVar "Nil"]]
      parseExpr "(let l2 (append 0 l))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "l2") [] [EApp (EApp (EVar "append") $ ENum 0) $ EVar "l"]]
      parseExpr "(ƒ map [f l] (match l (Cons h t => (Cons (f h) (map f t))) (Nil => Nil)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "map") [IdPattern "f", IdPattern "l"] [EPatternMatching (EVar "l") [Case (TConPattern "Cons" [IdPattern "h", IdPattern "t"]) [EApp (EApp (EVar "Cons") $ EApp (EVar "f") $ EVar "h") $ EApp (EApp (EVar "map") $ EVar "f") $ EVar "t"], Case (TConPattern "Nil" []) [EVar "Nil"]]]]
      parseExpr "(let l3 (map (λx => (= (% x 2) 0)) l))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "l3") [] [EApp (EApp (EVar "map") $ ELambda [Named "x" Nothing] Nothing [EApp (EApp (EVar "=") $ EApp (EApp (EVar "%") $ EVar "x") $ ENum 2) $ ENum 0]) $ EVar "l"]]
      parseExpr "(let patmat0 (match <\"a\" 3> (a => <\"ok\" a>)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "patmat0") [] [EPatternMatching (ETuple [EStr "a", ENum 3]) [Case (IdPattern "a") [ETuple [EStr "ok", EVar "a"]]]]]
      parseExpr "(let patmat1 (match <\"a\" 3> (<a b> => <\"ok\" a b>)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "patmat1") [] [EPatternMatching (ETuple [EStr "a", ENum 3]) [Case (TuplePattern [IdPattern "a", IdPattern "b"]) [ETuple [EStr "ok", EVar "a", EVar "b"]]]]]
      parseExpr "(let patmat2 (match <\"a\" 3> (<a _> => <\"ok\" a>)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "patmat2") [] [EPatternMatching (ETuple [EStr "a", ENum 3]) [Case (TuplePattern [IdPattern "a", WildcardPattern]) [ETuple [EStr "ok", EVar "a"]]]]]
      parseExpr "(ƒ k [x y] (match <x y> (<0 0> => 0) (_ => 1)))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "k") [IdPattern "x", IdPattern "y"] [EPatternMatching (ETuple [EVar "x", EVar "y"]) [Case (TuplePattern [NumPattern 0, NumPattern 0]) [ENum 0], Case WildcardPattern [ENum 1]]]]
      let name3 = "Ast"
      let dataType3 = TOper name3 []
      let numConstructor = TypeConstructor "Num" [intT]
      let addConstructor = TypeConstructor "Add" [dataType3, dataType3]
      let subConstructor = TypeConstructor "Sub" [dataType3, dataType3]
      let mulConstructor = TypeConstructor "Mul" [dataType3, dataType3]
      let divConstructor = TypeConstructor "Div" [dataType3, dataType3]
      let astData = EDataDecl name3 dataType3 [] [numConstructor, addConstructor, subConstructor, mulConstructor, divConstructor]
      parseExpr "(data Ast (Num Number) (Add Ast Ast) (Sub Ast Ast) (Mul Ast Ast) (Div Ast Ast))" `shouldBe` EProgram [astData]
      parseExpr "(ƒ eval [n] (match n (Num a => a) (Add a b => (+ (eval a) (eval b))) (Sub a b => (- (eval a) (eval b))) (Mul a b => (* (eval a) (eval b))) (Div a b => (/ (eval a) (eval b)))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "eval") [IdPattern "n"] [EPatternMatching (EVar "n") [Case (TConPattern "Num" [IdPattern "a"]) [EVar "a"],
                                                                                                                                                                                                                                                                                                          Case (TConPattern "Add" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "+") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"],
                                                                                                                                                                                                                                                                                                          Case (TConPattern "Sub" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "-") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"],
                                                                                                                                                                                                                                                                                                          Case (TConPattern "Mul" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "*") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"],
                                                                                                                                                                                                                                                                                                          Case (TConPattern "Div" [IdPattern "a", IdPattern "b"]) [EApp (EApp (EVar "/") $ EApp (EVar "eval") $ EVar "a") $ EApp (EVar "eval") $ EVar "b"]]]]
      parseExpr "(let sym (Mul (Add (Num 4) (Num 3)) (Sub (Num 4) (Num 1))))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "sym") [] [EApp (EApp (EVar "Mul") (EApp (EApp (EVar "Add") $ EApp (EVar "Num") $ ENum 4) $ EApp (EVar "Num") $ ENum 3)) (EApp (EApp (EVar "Sub") $ EApp (EVar "Num") $ ENum 4) $ EApp (EVar "Num") $ ENum 1)]]
      parseExpr "(let result (eval sym))" `shouldBe` EProgram [EDestructLetBinding (IdPattern "result") [] [EApp (EVar "eval") $ EVar "sym"]]
