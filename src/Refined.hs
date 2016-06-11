module Refined where

import Ast
import Type
import TypeScope
import Z3.Logic
import Z3.Assertion
import Prelude hiding (lookup)

genPred :: Term -> Z3Pred
genPred term = case term of
                 TmLT t1 t2 -> PAssert $ Less t1 t2
                 TmGT t1 t2 -> PAssert $ Greater t1 t2
                 TmLE t1 t2 -> PAssert $ LessE t1 t2
                 TmGE t1 t2 -> PAssert $ GreaterE t1 t2
                 TmEqual t1 t2 -> PAssert $ Equal t1 t2
                 TmAnd t1 t2 -> PConj (genPred t1) (genPred t2)
                 TmOr t1 t2 -> PDisj (genPred t1) (genPred t2)
                 TmNot t -> PNeg (genPred t)
                 _ -> error "not support"

replaceRtnTerm :: String -> Term -> Term -> Term
replaceRtnTerm rtnName rtnTerm predTerm = case predTerm of
                                         TmVar n -> if n == rtnName then rtnTerm else predTerm
                                         TmNum _ -> predTerm
                                         TmLT t1 t2 -> TmLT (replaceRtnTerm' t1) (replaceRtnTerm' t2)
                                         TmGT t1 t2 -> TmGT (replaceRtnTerm' t1) (replaceRtnTerm' t2)
                                         TmLE t1 t2 -> TmLE (replaceRtnTerm' t1) (replaceRtnTerm' t2)
                                         TmGE t1 t2 -> TmGE (replaceRtnTerm' t1) (replaceRtnTerm' t2)
                                         TmSub t1 t2 -> TmSub (replaceRtnTerm' t1) (replaceRtnTerm' t2)
                                         TmAdd t1 t2 -> TmAdd (replaceRtnTerm' t1) (replaceRtnTerm' t2)
                                         TmMul t1 t2 -> TmMul (replaceRtnTerm' t1) (replaceRtnTerm' t2)
                                         TmDiv t1 t2 -> TmDiv (replaceRtnTerm' t1) (replaceRtnTerm' t2)
                                         TmEqual t1 t2 -> TmEqual (replaceRtnTerm' t1) (replaceRtnTerm' t2)
                                         TmAnd t1 t2 -> TmAnd (replaceRtnTerm' t1) (replaceRtnTerm' t2)
                                         TmOr t1 t2 -> TmOr (replaceRtnTerm' t1) (replaceRtnTerm' t2)
                                         TmNot t -> TmNot (replaceRtnTerm' t)
                                         TmIf t1 t2 t3 -> TmIf (replaceRtnTerm' t1) (replaceRtnTerm' t2) (replaceRtnTerm' t3)
  where replaceRtnTerm' = replaceRtnTerm rtnName rtnTerm

genRtnPred :: String -> Term -> Term -> Z3Pred
-- use neg to find counterexamples
genRtnPred rtnName rtnTerm = PNeg . genPred . (replaceRtnTerm rtnName rtnTerm)

convertProg' :: Expr -> Term
convertProg' expr = case expr of
                      ENum n -> TmNum n
                      EVar name -> TmVar name
                      EApp fn arg -> case fn of
                                      EApp (EVar op) arg' -> opConstruct argTerm' argTerm
                                        where argTerm' = convertProg' arg'
                                              argTerm = convertProg' arg
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
                                                  "¬" -> let argTerm = convertProg' arg
                                                        in TmNot argTerm
                                                  _ -> error "not support"
                                      _ -> error "not support"
                      EIf cond (thenInstruction:[]) (elseInstruction:[]) -> TmIf condTerm thenTerm elseTerm
                        where condTerm = convertProg' cond
                              thenTerm = convertProg' thenInstruction
                              elseTerm = convertProg' elseInstruction
                      _ -> error "not support"

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
                             let rtnTerm = convertProg' instruction
                             case typeSig of
                               Just (TSig ta) -> do
                                 let terms = extractTerm ta
                                 let predNames = getPredNames ta
                                 case predNames of
                                   [] -> return PTrue
                                   _ -> case (argNames, terms) of
                                         ([n], [rtnTerm']) -> return $ PExists n RTInt $ genRtnPred' rtnTerm'
                                         ([n1, n2], [rtnTerm']) -> return $ PExists2 n1 n2 RTInt $ genRtnPred' rtnTerm'
                                         ([n], [argTerm, rtnTerm']) -> return $ PExists n RTInt $ PConj (genPred argTerm) $ genRtnPred' rtnTerm'
                                         ([n1, n2], [argTerm1, argTerm2, rtnTerm']) -> return $ PExists2 n1 n2 RTInt $ PConj (PConj (genPred argTerm1) $ genPred argTerm2) $ genRtnPred' rtnTerm'
                                         _ -> error "not support"
                                       where rtnName = last predNames
                                             genRtnPred' :: Term -> Z3Pred
                                             genRtnPred' = genRtnPred rtnName rtnTerm
                               -- always satisfied
                               _ -> return PTrue
                           _ -> error "not support"
