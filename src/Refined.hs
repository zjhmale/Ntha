module Refined where

import Ast
import Type
import TypeScope
import Z3.Logic
import Z3.Assertion
import Prelude hiding (lookup)

convertSig' :: Expr -> Term
convertSig' expr = case expr of
                     ENum n -> TmNum n
                     EVar name -> TmVar name
                     _ -> error "not support"

convertSig :: Expr -> Z3Pred
convertSig expr = case expr of
                    EApp fn arg -> case fn of
                                    EApp (EVar op) arg' -> case op of
                                                            "<" -> PAssert $ Less argTerm1' argTerm2'
                                                            ">" -> PAssert $ Greater argTerm1' argTerm2'
                                                            "≤" -> PAssert $ LessE argTerm1' argTerm2'
                                                            "≥" -> PAssert $ GreaterE argTerm1' argTerm2'
                                                            "=" -> PAssert $ Equal argTerm1' argTerm2'
                                                            "∧" -> PConj argTerm1 argTerm2
                                                            "∨" -> PDisj argTerm1 argTerm2
                                                            _ -> error "not support"
                                      where argTerm1' = convertSig' arg'
                                            argTerm2' = convertSig' arg
                                            argTerm1 = convertSig arg'
                                            argTerm2 = convertSig arg
                                    EVar op -> case op of
                                                "¬" -> PNeg argTerm
                                                _ -> error "not support"
                                      where argTerm = convertSig arg
                                    _ -> error "not support"
                    _ -> error "not support"

convertProg' :: Expr -> IO Term
convertProg' expr = case expr of
                      ENum n -> return $ TmNum n
                      EVar name -> return $ TmVar name
                      EApp fn arg -> case fn of
                                      EApp (EVar op) arg' -> do
                                        argTerm' <- convertProg' arg'
                                        argTerm <- convertProg' arg
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
                                        return $ opConstruct argTerm' argTerm
                                      EVar op -> do
                                        argTerm <- convertProg' arg
                                        case op of
                                          "¬" -> return $ TmNot argTerm
                                          _ -> error "not support"
                                      _ -> error "not support"
                      EIf cond (thenInstruction:[]) (elseInstruction:[]) -> do
                        condTerm <- convertProg' cond
                        thenTerm <- convertProg' thenInstruction
                        elseTerm <- convertProg' elseInstruction
                        return $ TmIf condTerm thenTerm elseTerm

convertProg :: Expr -> TypeScope -> IO Z3Pred
convertProg expr scope = case expr of
                           -- only support exists and exists2 for now
                           EDestructLetBinding main args (instruction:[]) -> do
                             let name = case main of
                                          IdPattern n -> n
                                          _ -> ""
                             let typeSig = lookup name scope
                             let argNames = map (\pat -> case pat of
                                                          IdPattern n -> n
                                                          _ -> error "not support")
                                                args
                             instrTerm <- convertProg instruction scope
                             case typeSig of
                               Just (TSig ta) -> do
                                 let preds = extractPred ta
                                 case (argNames, preds) of
                                   ([n], [rtnPred]) -> return $ PExists n RTInt $ PNeg rtnPred
                                   ([n1, n2], [rtnPred]) -> return $ PExists2 n1 n2 RTInt $ PNeg rtnPred
                                   ([n], [argPred, rtnPred]) -> return $ PExists n RTInt $ PConj argPred $ PNeg rtnPred
                                   ([n1, n2], [argPred1, argPred2, rtnPred]) -> return $ PExists2 n1 n2 RTInt $ PConj (PConj argPred1 argPred2) $ PNeg rtnPred
                                   _ -> error "not support"
                               -- always satisfied
                               _ -> return PTrue
                           _ -> error "not support"
