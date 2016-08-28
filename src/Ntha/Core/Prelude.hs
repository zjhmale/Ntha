module Ntha.Core.Prelude where

import           Ntha.Core.Ast
import           Ntha.Runtime.Value
import           Ntha.State
import           Ntha.Type.Type
import           Ntha.Type.TypeScope

import           Debug.Trace
import qualified Data.Map            as M

mkTCon :: TypeConstructor -> Expr -> Type
mkTCon (TypeConstructor name types) (EDataDecl _ t _ _) = TCon name types t
mkTCon _ _ = error "not support"

assumptions :: Infer TypeScope
assumptions = do
  tvarA <- makeVariable
  tvarB <- makeVariable
  let name = "List"
  let vars = [tvarA]
  let dataType = TOper name vars
  let consConstructor = TypeConstructor "Cons" [tvarA, TOper "List" [tvarA]]
  let nilConstructor = TypeConstructor "Nil" []
  let listData = EDataDecl "List" dataType vars [consConstructor, nilConstructor]
  return $ TypeScope Nothing $
    M.fromList [("+", functionT [intT, intT] intT),
                ("-", functionT [intT, intT] intT),
                ("*", functionT [intT, intT] intT),
                ("/", functionT [intT, intT] intT),
                ("%", functionT [intT, intT] intT),
                ("=", functionT [tvarB, tvarB] boolT),
                ("≠", functionT [tvarB, tvarB] boolT),
                ("<", functionT [intT, intT] boolT),
                (">", functionT [intT, intT] boolT),
                ("≤", functionT [intT, intT] boolT),
                ("≥", functionT [intT, intT] boolT),
                ("∧", functionT [boolT, boolT] boolT),
                ("∨", functionT [boolT, boolT] boolT),
                ("¬", functionT [boolT] boolT),
                ("int2str", functionT [intT] strT),
                ("bool2str", functionT [boolT] strT),
                ("asserteq", functionT [tvarB, tvarB] unitT),
                ("print", functionT [strT] unitT),
                ("error", functionT [strT] tvarB),
                ("reverse", functionT [listT tvarB] (listT tvarB)),
                ("list?", functionT [tvarB] boolT),
                ("string?", functionT [tvarB] boolT),
                ("Cons", mkTCon consConstructor listData),
                ("Nil", mkTCon nilConstructor listData),
                ("inc", functionT [intT] intT),
                ("dec", functionT [intT] intT)]

builtins :: ValueScope
builtins = ValueScope Nothing $
  M.fromList [("+", binFn (\(VNum a) (VNum b) -> (VNum $ a + b))),
              ("-", binFn (\(VNum a) (VNum b) -> (VNum $ a - b))),
              ("*", binFn (\(VNum a) (VNum b) -> (VNum $ a * b))),
              ("/", binFn (\(VNum a) (VNum b) -> (VNum $ a `div` b))),
              ("%", binFn (\(VNum a) (VNum b) -> (VNum $ a `mod` b))),
              ("=", binFn (\a b -> VBool $ a == b)),
              ("≠", binFn (\a b -> VBool $ a /= b)),
              ("<", binFn (\a b -> VBool $ a < b)),
              (">", binFn (\a b -> VBool $ a > b)),
              ("≤", binFn (\a b -> VBool $ a <= b)),
              ("≥", binFn (\a b -> VBool $ a >= b)),
              ("∧", binFn (\(VBool a) (VBool b) -> VBool $ a && b)),
              ("∨", binFn (\(VBool a) (VBool b) -> VBool $ a || b)),
              ("¬", Fn (\(VBool b) _ -> VBool $ not b)),
              ("int2str", Fn (\(VNum n) _ -> strV $ show n)),
              ("bool2str", Fn (\(VBool b) _ -> strV $ show b)),
              ("asserteq", binFn (\a b -> if a == b
                                         then VUnit
                                         else error $ show a ++ " and " ++ show b ++ " not equal.")),
              ("print", Fn (\v _ -> trace (desugerStrV v) VUnit)),
              ("error", Fn (\v _ -> error $ desugerStrV v)),
              ("reverse", Fn (\v _ -> reverseList v)),
              ("list?", Fn (\v _ -> case v of
                                     Adt "Cons" _ -> VBool True
                                     _ -> VBool False)),
              ("string?", Fn (\v _ -> VBool $ isString v)),
              ("Cons", binFn (\a b -> cons a b)),
              ("Nil", nil),
              ("inc", Fn (\(VNum n) _ -> VNum $ n + 1)),
              ("dec", Fn (\(VNum n) _ -> VNum $ n - 1))]
