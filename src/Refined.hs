module Refined where

import Ast
import TypeScope
import Z3.Class
import Z3.Assertion
import Z3.Monad

import qualified Data.Map as M

data Term = TmVar   String
          | TmNum   Int
          | TmBool  Bool
          | TmLT    Term Term
          | TmGT    Term Term
          | TmLE    Term Term
          | TmGE    Term Term
          | TmSub   Term Term
          | TmAdd   Term Term
          | TmMul   Term Term
          | TmDiv   Term Term
          | TmEqual Term Term
          | TmAnd   Term Term
          | TmOr    Term Term
          | TmNot   Term
          | TmIf    Term Term Term

deriving instance Eq Term

data Type = TyBool
          | TyInt

deriving instance Eq Type

instance Z3Encoded Term where
    encode (TmVar x) = do
        ctx <- getQualifierCtx
        case M.lookup x ctx of
            Just (idx, _) -> return idx
            Nothing -> smtError $ "Can't find variable " ++ x
    encode (TmNum n) = mkIntSort >>= mkInt n
    encode (TmBool b) = mkBool b
    encode (TmLT t1 t2) = encode (Less t1 t2)
    encode (TmGT t1 t2) = encode (Greater t1 t2)
    encode (TmLE t1 t2) = encode (LessE t1 t2)
    encode (TmGE t1 t2) = encode (GreaterE t1 t2)
    encode (TmAdd t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkAdd [a1, a2]
    encode (TmSub t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkSub [a1, a2]
    encode (TmMul t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkMul [a1, a2]
    encode (TmDiv t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkDiv a1 a2
    encode (TmEqual t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkEq a1 a2
    encode (TmAnd t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkAnd [a1, a2]
    encode (TmOr t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkOr [a1, a2]
    encode (TmNot t) = encode t >>= mkNot
    encode (TmIf p c a) = do
        a1 <- encode p
        a2 <- encode c
        a3 <- encode a
        mkIte a1 a2 a3

instance Z3Sorted Term where
    sort (TmVar x) = do
        ctx <- getQualifierCtx
        case M.lookup x ctx of
            Just (_, s) -> return s
            Nothing -> smtError $ "Can't find variable " ++ x
    sort (TmNum _) = mkIntSort
    sort (TmBool _) = mkBoolSort
    sort (TmLT _ _) = mkBoolSort
    sort (TmGT _ _) = mkBoolSort
    sort (TmLE _ _) = mkBoolSort
    sort (TmGE _ _) = mkBoolSort
    sort (TmAdd _ _) = mkIntSort
    sort (TmSub _ _) = mkIntSort
    sort (TmMul _ _) = mkIntSort
    sort (TmDiv _ _) = mkIntSort
    sort (TmEqual _ _) = mkBoolSort
    sort (TmAnd _ _) = mkBoolSort
    sort (TmOr _ _) = mkBoolSort
    sort (TmNot _) = mkBoolSort
    sort (TmIf _ c _) = sort c

instance Z3Sorted Type where
    sort TyBool = mkBoolSort
    sort TyInt  = mkIntSort

convert :: Expr -> TypeScope -> IO (TypeScope, Term)
convert term scope = case term of
                       ENum n -> return (scope, TmNum n)
                       EBool b -> return (scope, TmBool b)
                       EVar name -> return (scope, TmVar name)
                       EApp fn arg -> case fn of
                                       EApp (EVar op) arg' -> do
                                         (_, argTerm') <- convert arg' scope
                                         (_, argTerm) <- convert arg scope
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
                                         (_, argTerm) <- convert arg scope
                                         case op of
                                           "¬" -> return (scope, TmNot argTerm)
                                           _ -> error "not support"
                                       _ -> error "not support"
                       EIf cond thenInstructions elseInstructions -> do
                         (_, condTerm) <- convert cond scope
                         (_, thenTerm) <- convert (thenInstructions!!0) scope
                         (_, elseTerm) <- convert (elseInstructions!!0) scope
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
