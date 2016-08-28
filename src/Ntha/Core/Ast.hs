module Ntha.Core.Ast where

import           Ntha.Type.Type

import           Data.List        (intercalate)
import           Data.Maybe       (fromMaybe)
import qualified Data.Map         as M
import qualified Text.PrettyPrint as PP

type EName = String -- variable name
type EPath = String
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
          | ELetBinding Pattern Expr [Expr]
          | EDestructLetBinding Pattern [Pattern] [Expr]
          | EDataDecl EName Type [TypeVariable] [TypeConstructor]
          | ETypeSig EName Type -- explicit type annotation
          | EImport EPath
          | EProgram [Expr]
          deriving (Eq, Ord)

isImport :: Expr -> Bool
isImport expr = case expr of
  EImport _ -> True
  _ -> False

-- for do block desuger to bind
data Bind = Bind EName Expr
          | Return Expr
          | Single Expr

-- for cond desuger to if
data Clause = Clause Expr Expr
            | Else Expr

data TypeConstructor = TypeConstructor EName [Type]
                       deriving (Eq, Ord)

data Named = Named EName (Maybe Type)
             deriving (Eq, Ord)

data Pattern = WildcardPattern
             | IdPattern EName
             | NumPattern Int
             | BoolPattern Bool
             | CharPattern Char
             | TuplePattern [Pattern]
             | TConPattern EName [Pattern]
             deriving (Eq, Ord)

data Case = Case Pattern [Expr]
            deriving (Eq, Ord)

-- temp structure for parser
data EVConArg = EVCAVar EName
              | EVCAOper EName [EName]
              | EVCAList EVConArg
              | EVCATuple [EVConArg]
              deriving (Show, Eq, Ord)

data EVConstructor = EVConstructor EName [EVConArg]
                     deriving (Show, Eq, Ord)

substName :: M.Map EName EName -> Expr -> Expr
substName subrule (EVar name) = EVar $ fromMaybe name $ M.lookup name subrule
substName subrule (EAccessor expr field) = EAccessor (substName subrule expr) field
substName subrule (EList exprs) = EList $ map (substName subrule) exprs
substName subrule (ETuple exprs) = ETuple $ map (substName subrule) exprs
substName subrule (ERecord pairs) = ERecord $ M.map (substName subrule) pairs
substName subrule (ELambda nameds t exprs) = ELambda newNames t newExprs
  where
  newNames = map (\(Named name t') -> Named (fromMaybe name $ M.lookup name subrule) t') nameds
  newExprs = map (substName subrule) exprs
substName subrule (EApp fn arg) = EApp (substName subrule fn) (substName subrule arg)
substName subrule (EIf cond thenInstrs elseInstrs) = EIf newCond newThenInstrs newElseInstrs
  where
  newCond = substName subrule cond
  newThenInstrs = map (substName subrule) thenInstrs
  newElseInstrs = map (substName subrule) elseInstrs
substName subrule (EPatternMatching expr cases) = EPatternMatching newExpr newCases
  where
  newCases = map (\(Case pat exprs) -> Case pat (map (substName subrule) exprs)) cases
  newExpr = substName subrule expr
substName subrule (ELetBinding pat expr exprs) = ELetBinding pat (substName subrule expr) $ map (substName subrule) exprs
substName _ e = e

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
  ELambda params annoT body -> "λ" ++ stringofNameds params ++ b
    where b = (case annoT of
                 Just annoT' -> " : " ++ show annoT'
                 Nothing -> "")
              ++ " = \n" ++ intercalate "" (map (\instr -> "\t" ++ show instr ++ "\n") body)
  EIf cond thenInstrs elseInstrs -> "if " ++ show cond ++ " then \n" ++ th ++ "else \n" ++ el
    where stringOfInstrs instrs = intercalate "" $ map (\instr -> "\t" ++ show instr ++ "\n") instrs
          th = stringOfInstrs thenInstrs
          el = stringOfInstrs elseInstrs
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
  ELambda params annoT body -> tab i ++ "λ" ++ stringofNameds params ++ b
    where b = (case annoT of
                 Just annoT' -> " : " ++ show annoT'
                 Nothing -> "")
              ++ " = \n" ++ intercalate "" (map (\instr -> "\t" ++ reprOfExpr (i + 1) instr ++ "\n") body)
  EIf cond thenInstrs elseInstrs -> tab i ++ "if " ++ show cond ++ " then \n" ++ th ++ tab i ++ "else \n" ++ el
    where stringOfInstrs instrs = intercalate "" $ map (\instr -> "\t" ++ reprOfExpr (i + 1) instr ++ "\n") instrs
          th = stringOfInstrs thenInstrs
          el = stringOfInstrs elseInstrs
  EPatternMatching input cases -> tab i ++ "match " ++ show input ++ stringOfCases i cases
  EDataDecl name _ tvars tcons -> tab i ++ "data " ++ name ++ " " ++ unwords (map show tvars) ++ " = " ++ cs
    where scons = (map (\(TypeConstructor name' types) ->
                          name' ++ case types of
                                    [] -> ""
                                    _ -> " " ++ unwords (map show types)) tcons)
          cs = intercalate " | " scons
  EDestructLetBinding main args instrs -> tab i ++ "let " ++ show main ++ " " ++ unwords (map show args) ++ " = \n" ++ is
    where is = intercalate "" (map (\instr -> reprOfExpr (i + 1) instr ++ "\n") instrs)
  ELetBinding main def body -> tab i ++ "let " ++ show main ++ " " ++ show def ++ " in " ++ intercalate "\n" (map show body)
  ETypeSig name t -> tab i ++ "(" ++ name ++ " : " ++ show t ++ ")"
  EImport path -> "import " ++ path
  EProgram instrs -> intercalate "" $ map (\instr -> reprOfExpr i instr ++ "\n") instrs

instance Show Expr where
    showsPrec _ x = shows $ PP.text $ stringOfExpr x

instance Show Pattern where
    show WildcardPattern = "_"
    show (NumPattern val) = "pint→" ++ show val
    show (BoolPattern val) = "pbool→" ++ show val
    show (CharPattern val) = "pchar→" ++ show val
    show (IdPattern name) = "'" ++ name ++ "'"
    show (TuplePattern pattens) = "(" ++ intercalate "," (map show pattens) ++ ")"
    show (TConPattern name pattens) = name ++ " " ++ show pattens
