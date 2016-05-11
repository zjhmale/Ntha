module Prologue where

import Ast
import Type
import Value
import State
import TypeScope
import qualified Data.Map as M

mkTCon :: TypeConstructor -> Expr -> Type
mkTCon (TypeConstructor name types) (EDataDecl _ t _ _) = TCon name types t

assumptions :: Infer TypeScope
assumptions = do
  tvarA <- makeVariable
  let name = "List"
  let vars = [tvarA]
  let dataType = TOper name vars
  let consConstructor = TypeConstructor "Cons" [tvarA, TOper "List" [tvarA]]
  let nilConstructor = TypeConstructor "Nil" []
  let listData = EDataDecl "List" dataType vars [consConstructor, nilConstructor]
  return $ TypeScope Nothing $ M.fromList [("+", functionT [intT, intT] intT),
                                           ("-", functionT [intT, intT] intT),
                                           ("Cons", mkTCon consConstructor listData),
                                           ("Nil", mkTCon nilConstructor listData),
                                           ("inc", functionT [intT] intT),
                                           ("dec", functionT [intT] intT)]

builtins :: ValueScope
builtins = ValueScope Nothing $ M.fromList [("+", binFn (\(VNum a) (VNum b) -> (VNum $ a + b))),
                                            ("-", binFn (\(VNum a) (VNum b) -> (VNum $ a - b))),
                                            ("inc", Fn (\(VNum n) _ -> VNum $ n + 1)),
                                            ("dec", Fn (\(VNum n) _ -> VNum $ n - 1))]