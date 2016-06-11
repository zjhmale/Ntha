module Refined where

import Ast
import Type
import TypeScope

convertSig :: Expr -> Term
convertSig term = case term of
                    ENum n -> TmNum n
                    EBool b -> TmBool b
                    EVar name -> TmVar name
                    EApp fn arg -> case fn of
                                    EApp (EVar op) arg' -> opConstruct argTerm' argTerm
                                      where argTerm' = convertSig arg'
                                            argTerm = convertSig arg
                                            opConstruct = case op of
                                                            "+" -> TmAdd
                                                            "-" -> TmSub
                                                            "*" -> TmMul
                                                            "/" -> TmDiv
                                                            "<" -> TmLT
                                                            ">" -> TmGT
                                                            "≤" -> TmLE
                                                            "≥" -> TmGE
                                                            "=" -> TmEqual
                                                            "∧" -> TmAdd
                                                            "∨" -> TmOr
                                                            _ -> error "not support"
                                    EVar op -> case op of
                                                "¬" -> TmNot argTerm
                                                _ -> error "not support"
                                      where argTerm = convertSig arg
                                    _ -> error "not support"
                    EIf cond (thenInstruction:[]) (elseInstruction:[]) -> TmIf condTerm thenTerm elseTerm
                      where condTerm = convertSig cond
                            thenTerm = convertSig thenInstruction
                            elseTerm = convertSig elseInstruction
                    _ -> error "not support"

convertProg :: Expr -> TypeScope -> IO (TypeScope, Term)
convertProg term scope = case term of
                           ENum n -> return (scope, TmNum n)
                           EBool b -> return (scope, TmBool b)
                           EVar name -> return (scope, TmVar name)
                           EApp fn arg -> case fn of
                                           EApp (EVar op) arg' -> do
                                             (_, argTerm') <- convertProg arg' scope
                                             (_, argTerm) <- convertProg arg scope
                                             let opConstruct = case op of
                                                                 "+" -> TmAdd
                                                                 "-" -> TmSub
                                                                 "*" -> TmMul
                                                                 "/" -> TmDiv
                                                                 "<" -> TmLT
                                                                 ">" -> TmGT
                                                                 "≤" -> TmLE
                                                                 "≥" -> TmGE
                                                                 "=" -> TmEqual
                                                                 "∧" -> TmAdd
                                                                 "∨" -> TmOr
                                                                 _ -> error "not support"
                                             return (scope, opConstruct argTerm' argTerm)
                                           EVar op -> do
                                             (_, argTerm) <- convertProg arg scope
                                             case op of
                                               "¬" -> return (scope, TmNot argTerm)
                                               _ -> error "not support"
                                           _ -> error "not support"
                           EIf cond (thenInstruction:[]) (elseInstruction:[]) -> do
                             (_, condTerm) <- convertProg cond scope
                             (_, thenTerm) <- convertProg thenInstruction scope
                             (_, elseTerm) <- convertProg elseInstruction scope
                             return (scope, TmIf condTerm thenTerm elseTerm)
                           -- only support exists and exists2 for now
                           {-EDestructLetBinding main args (instruction:[]) -> do
                             let name = case main of
                                          IdPattern n -> n
                                          _ -> ""
                             let typeSig = lookup name scope
                             instrTerm <- convertProg instruction scope
                             case typeSig of
                               Just (TSig ta) -> unify ta letT
                               _ -> return (scope, instrTerm)-}
                           _ -> error "not support"
