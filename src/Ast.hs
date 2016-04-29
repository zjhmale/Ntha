{-# OPTIONS_GHC -Wall #-}

module Ast where

import Type
import qualified Data.Map as M

type VName = String -- variable name
type EField = String

data AstNode = Program [Instruction]
             | Instruction

data Instruction = Expr
                 | LetBinding VName (Maybe Type) [Named] [Instruction]
                 | DestructLetBinding Pattern [Pattern] [Instruction]
                 | DataDecl VName Type [Type] [TypeConstructor]
                 | ExceptionDecl VName [Type]

data TypeConstructor = TypeConstructor [VName] (Maybe [Type])

data Named = Named VName (Maybe Type)

data Pattern = WildcardPattern
             | IdPattern VName
             | TuplePattern [Pattern]
             | TyConPattern VName [Pattern]

data Case = Case Pattern [Instruction]

data Expr =
    EVar VName
  | EAccessor Expr EField
  | ENum Int
  | EStr String
  | EChar Char
  | EBool Bool
  | EList [Expr]
  | ETuple [Expr]
  | ERecord (M.Map EField Expr)
  | EUnit
  | ELambda [Named] (Maybe Type) [Instruction]
  | EApp Expr Expr
  | ETryCatch [Instruction] [Case]
  | EThrow Expr
  | EIf Expr Expr Expr
  | PatternMatching Expr [Case]

instance Show Expr where
    show (EVar n) = n
    show (EAccessor e f) = show e ++ "." ++ f
    show (ENum v) = show v
    show (EStr s) = s
    show (EChar c) = show c
