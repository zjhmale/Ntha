{-# OPTIONS_GHC -Wall #-}

module Eval where

import Ast
import Value
import ValueScope
import Prelude hiding (lookup)
import qualified Data.Map as M

eval :: Expr -> ValueScope -> Value
eval expr scope = case expr of
                    ENum v -> VNum v
                    EBool v -> VBool v
                    EChar v -> VChar v
                    EUnit -> VUnit
                    EVar name -> case lookup name scope of
                                  Just val -> val
                                  Nothing -> error $ "Unknwon identifier " ++ show expr
                    EAccessor obj field -> case eval obj scope of
                                            VRecord pairs -> case M.lookup field pairs of
                                                              Just val -> val
                                                              Nothing -> error $ "No field " ++ field ++ "in "++ show obj
                                            _ -> error $ "Not a record " ++ show obj
