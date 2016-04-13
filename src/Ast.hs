{-# OPTIONS_GHC -Wall #-}

module Ast where

type VName = String -- variable name

data Lit =
    LInt Int
  | LBool Bool
  | LString String

data Expr =
    EVar VName
  | ELit Lit
  | EAbs VName Expr
  | EApp Expr Expr
  | ELet VName Expr Expr
  | ELetRec VName Expr Expr
  | ESucc Expr
  | EPred Expr
  | EAdd Expr Expr
  | ETimes Expr Expr
  | EIsZero Expr
  | EIf Expr Expr Expr
  | EFix Expr
  | EId Expr
  | ENot Expr
  | EAnd Expr Expr
  | EEq Expr Expr
  | ECompose Expr Expr
  | ENil
  | ECons Expr Expr
  | EHead Expr
  | ETail Expr
  | EIsEmpty Expr
  | EPair Expr Expr

instance Show Lit where
    show (LInt value) = show value
    show (LBool value) = show value
    show (LString value) = show value

instance Show Expr where
    show (EVar name) = name
    show (ELit lit) = show lit
    show (EAbs name e) = "λ" ++ name ++ " → " ++ show e
    show (EApp lexpr rexpr) = show lexpr ++ "(" ++ show rexpr ++ ")"
    show (ELet name expr body) = "let " ++ name ++ " = " ++ show expr ++ " in " ++ show body
    show (ELetRec name expr body) = "let rec " ++ name ++ " = " ++ show expr ++ " in " ++ show body
    show (ESucc num) = "succ " ++ show num
    show (EPred num) = "pred " ++ show num
    show (EAdd lnum rnum) = show lnum ++ " + " ++ show rnum
    show (ETimes lnum rnum) = show lnum ++ " * " ++ show rnum
    show (EIsZero num) = show "zero? " ++ show num
    show (EIf p c a) = "if " ++ show p ++ " then " ++ show c ++ " else " ++ show a
    show (EFix lambda) = "fix(" ++ show lambda ++ ")"
    show (EId e) = "id(" ++ show e ++ ")"
    show (ENot e) = "¬ " ++ show e
    show (EAnd lexpr rexpr) = show lexpr ++ " ∧ " ++ show rexpr
    show (EEq lexpr rexpr) = show lexpr ++ " ≡ " ++ show rexpr
    show (ECompose lfun rfun) = show lfun ++ " ∘ " ++ show rfun
    show ENil = "[]"
    show (ECons h t) = show h ++ " :: " ++ show t
    show (EHead l) = "head " ++ show l
    show (ETail l) = "tail " ++ show l
    show (EIsEmpty l) = "empty? " ++ show l
    show (EPair lexpr rexpr) = "(" ++ show lexpr ++ ", " ++ show rexpr ++ ")"
