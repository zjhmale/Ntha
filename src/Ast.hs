{-# OPTIONS_GHC -Wall #-}

module Ast where

import Type
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Text.PrettyPrint as PP

type EName = String -- variable name
type EField = String
type EIndent = Int
type TypeVariable = Type -- just for documentation

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
          | EIf Expr [Expr] [Expr]
          | EPatternMatching Expr [Case]
          | ELetBinding EName (Maybe Type) [Named] [Expr]
          | EDestructLetBinding Pattern [Pattern] [Expr]
          | EDataDecl EName Type [TypeVariable] [TypeConstructor]
          | EProgram [Expr]
          deriving (Eq, Ord)

data TypeConstructor = TypeConstructor EName [Type]
                       deriving (Eq, Ord)

data Named = Named EName (Maybe Type)
             deriving (Eq, Ord)

data Pattern = WildcardPattern
             | IdPattern EName
             | TuplePattern [Pattern]
             | TConPattern EName [Pattern]
             deriving (Eq, Ord)

data Case = Case Pattern [Expr]
            deriving (Eq, Ord)

tab :: EIndent -> String
tab i = intercalate "" $ take i $ repeat "\t"

stringOfNamed :: Named -> String
stringOfNamed (Named name t) = name ++ case t of
                                        Just t' -> ":" ++ show t'
                                        Nothing -> ""

stringofNameds :: [Named] -> String
stringofNameds = unwords . (map stringOfNamed)

stringOfExpr :: Expr -> String
stringOfExpr e = case e of
                  EApp fn arg -> "<" ++ show fn ++ ">(" ++ show arg ++ ")"
                  ELambda params annoT body -> "λ" ++ stringofNameds params ++ (case annoT of
                                                                                Just annoT' -> " : " ++ show annoT'
                                                                                Nothing -> "") ++ " = \n" ++ intercalate "" (map (\instr -> "\t" ++ show instr ++ "\n") body)
                  EIf cond thenInstrs elseInstrs -> "if " ++ show cond ++ " then \n" ++ stringOfInstrs thenInstrs ++ "else \n" ++ stringOfInstrs elseInstrs where
                    stringOfInstrs instrs = intercalate "" $ map (\instr -> "\t" ++ show instr ++ "\n") instrs
                  ELetBinding name t args instrs -> "let " ++ name ++ " " ++ stringofNameds args ++ (case t of
                                                                                                      Just t' -> " : " ++ show t'
                                                                                                      Nothing -> "") ++ " = \n" ++ intercalate "" (map (\instr -> "\t" ++ show instr ++ "\n") instrs)
                  EProgram instrs -> intercalate "" $ map (\instr -> show instr ++ "\n") instrs
                  _ -> reprOfExpr 0 e

stringOfCase :: EIndent -> Case -> String
stringOfCase i (Case pat outcomes) = "\n" ++ tab i ++ show pat ++ " ⇒ " ++ show outcomes

stringOfCases :: EIndent -> [Case] -> String
stringOfCases i cases = intercalate "" (map (stringOfCase i) cases)

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
                  ERecord pairs -> "{" ++ intercalate "," (M.elems $ M.mapWithKey (\f v -> f ++ ": " ++ reprOfExpr 0 v) pairs) ++ "}"
                  EApp _ _ -> tab i ++ show e
                  ELambda params annoT body -> tab i ++ "λ" ++ stringofNameds params ++ (case annoT of
                                                                                         Just annoT' -> " : " ++ show annoT'
                                                                                         Nothing -> "") ++ " = \n" ++ intercalate "" (map (\instr -> "\t" ++ reprOfExpr (i + 1) instr ++ "\n") body)
                  EIf cond thenInstrs elseInstrs -> tab i ++ "if " ++ show cond ++ " then \n" ++ stringOfInstrs thenInstrs ++ tab i ++ "else \n" ++ stringOfInstrs elseInstrs where
                    stringOfInstrs instrs = intercalate "" $ map (\instr -> "\t" ++ reprOfExpr (i + 1) instr ++ "\n") instrs
                  EPatternMatching input cases -> tab i ++ "match " ++ show input ++ stringOfCases i cases
                  EDataDecl name _ tvars tcons -> tab i ++ "data " ++ name ++ " " ++ unwords (map show tvars) ++ " = " ++ intercalate " | " (map (\(TypeConstructor name' types) -> name' ++ case types of
                                                                                                                                                                                            [] -> ""
                                                                                                                                                                                            _ -> " " ++ unwords (map show types)) tcons)
                  EDestructLetBinding main args instrs -> tab i ++ "let " ++ show main ++ " " ++ unwords (map show args) ++ " = \n" ++ intercalate "" (map (\instr -> reprOfExpr (i + 1) instr ++ "\n") instrs)
                  ELetBinding name t args instrs -> tab i ++ "let " ++ name ++ " " ++ stringofNameds args ++ (case t of
                                                                                                              Just t' -> " : " ++ show t'
                                                                                                              Nothing -> "") ++ " = \n" ++ intercalate "" (map (\instr -> reprOfExpr (i + 1) instr ++ "\n") instrs)

                  EProgram instrs -> intercalate "" $ map (\instr -> reprOfExpr i instr ++ "\n") instrs

instance Show Expr where
    showsPrec _ x = shows $ PP.text $ stringOfExpr x

instance Show Pattern where
    show WildcardPattern = "_"
    show (IdPattern name) = "'" ++ name ++ "'"
    show (TuplePattern pattens) = "(" ++ intercalate "," (map show pattens) ++ ")"
    show (TConPattern name pattens) = name ++ " " ++ show pattens
