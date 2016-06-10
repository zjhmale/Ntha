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
                    EIf cond thenInstructions elseInstructions -> TmIf condTerm thenTerm elseTerm
                      where condTerm = convertSig cond
                            thenTerm = convertSig (thenInstructions!!0)
                            elseTerm = convertSig (elseInstructions!!0)
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
                           EIf cond thenInstructions elseInstructions -> do
                             (_, condTerm) <- convertProg cond scope
                             (_, thenTerm) <- convertProg (thenInstructions!!0) scope
                             (_, elseTerm) <- convertProg (elseInstructions!!0) scope
                             return (scope, TmIf condTerm thenTerm elseTerm)
                           {-
                           EDestructLetBinding main args instructions -> do
                             let name = case main of
                                          IdPattern n -> n
                                          _ -> ""
                             let typeSig = lookup name scope
                             let newScope = child scope
                             (newScope', newNonGeneric, letTV) <- visitPattern main newScope nonGeneric
                             let newNonGeneric' = S.insert letTV newNonGeneric
                             (argTypes, newScope'', newNonGeneric'') <- foldM (\(types, env, nonGen) arg -> do
                                                                               (newEnv, newNonGen, argT) <- visitPattern arg env nonGen
                                                                               return (types ++ [argT], newEnv, newNonGen))
                                                                              ([], newScope', newNonGeneric') args
                             rtnT <- foldM (\_ instr -> snd <$> analyze instr newScope'' newNonGeneric'') unitT instructions
                             let letT = functionT argTypes rtnT
                             newScope''' <- definePattern main letT newScope''
                             case typeSig of
                               Just (TSig ta) -> unify ta letT
                               _ -> return ()
                             return (newScope''', letT)
                           -}
                           --ETypeSig name t -> return (insert name (TSig t) scope, unitT)
                           _ -> error "not support"
