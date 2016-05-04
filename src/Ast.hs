{-# OPTIONS_GHC -Wall #-}

module Ast where

import Type
import Data.List(intercalate)
import qualified Data.Map as M
import qualified Text.PrettyPrint as PP

type EName = String -- variable name
type EField = String
type EIndent = Int

data Expr = EVar EName
          | EAccessor Expr EField
          | ENum Int
          | EStr String
          | EChar Char
          | EBool Bool
          | EList [Expr]
          | ETuple [Expr]
          | ERecord (M.Map EField Expr)
          | EUnit
          | ELambda [Named] (Maybe Type) [Expr]
          | EApp Expr Expr
          | ETryCatch [Expr] [Case]
          | EThrow Expr
          | EIf Expr [Expr] [Expr]
          | EPatternMatching Expr [Case]
          | ELetBinding EName (Maybe Type) [Named] [Expr]
          | EDestructLetBinding Pattern [Pattern] [Expr]
          | EDataDecl EName Type [Type] [TypeConstructor]
          | EExceptionDecl EName [Type]
          | EProgram [Expr]

data TypeConstructor = TypeConstructor [EName] (Maybe [Type])

data Named = Named EName (Maybe Type)

data Pattern = WildcardPattern
             | IdPattern EName
             | TuplePattern [Pattern]
             | TConPattern EName [Pattern]

data Case = Case Pattern [Expr]

tab :: EIndent -> String
tab i = intercalate "" $ take i $ repeat "\t"

stringOfExpr :: Expr -> String
stringOfExpr e = case e of
                  EVar _ -> reprOfExpr 0 e
                  EAccessor _ _ -> reprOfExpr 0 e
                  ENum _ -> reprOfExpr 0 e
                  EStr _ -> reprOfExpr 0 e
                  EChar _ -> reprOfExpr 0 e
                  EUnit -> reprOfExpr 0 e
                  EBool _ -> reprOfExpr 0 e
                  EList _ -> reprOfExpr 0 e
                  ETuple _ -> reprOfExpr 0 e
                  ERecord _ -> reprOfExpr 0 e

reprOfExpr :: EIndent -> Expr -> String
reprOfExpr i e = case e of
                    EVar n -> tab i ++ n
                    EAccessor e' f -> tab i ++ reprOfExpr 0 e' ++ "." ++ f
                    ENum v -> tab i ++ show v
                    EStr v -> tab i ++ v
                    EChar v -> tab i ++ [v]
                    EBool v -> tab i ++ show v
                    EUnit -> tab i ++ "()"
                    EList es -> tab i ++ show es
                    ETuple es -> "(" ++ intercalate "," (map (reprOfExpr 0) es) ++ ")"
                    ERecord pairs -> "{" ++ intercalate "," (M.keys $ M.mapWithKey (\f v -> f ++ ": " ++ reprOfExpr 0 v) pairs) ++ "}"

instance Show Expr where
    show (EVar n) = n
    show (EAccessor e f) = show e ++ "." ++ f
    show (ENum v) = show v
    show (EStr s) = s
    show (EChar c) = show c

instance Show Pattern where
    show WildcardPattern = "_"
