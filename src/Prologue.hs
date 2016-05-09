module Prologue where

import Ast
import Type
import Value
import State
import TypeScope
import qualified Data.Map as M

assumptions :: Infer TypeScope
assumptions = do
  return $ TypeScope Nothing $ M.fromList [("+", functionT [intT, intT] intT),
                                           ("-", functionT [intT, intT] intT)]

builtins :: ValueScope
builtins = ValueScope Nothing $ M.fromList [("+", binFn (\(VNum a) (VNum b) -> (VNum $ a + b))),
                                            ("-", binFn (\(VNum a) (VNum b) -> (VNum $ a - b)))]