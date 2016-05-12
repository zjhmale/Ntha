{
module Parser where

import Ast
import Type
import Lexer
import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)
}

%name expr
%tokentype { Token }
%error { parseError }

%token
    data     { DATA }
    match    { MATCH }
    defun    { DEFUN }
    lambda   { LAMBDA }
    arrow    { ARROW }
    con      { CON $$ }
    '['      { LBRACKET }
    ']'      { RBRACKET }
    '('      { LPAREN }
    ')'      { RPAREN }
    '<'      { LANGLEBRACKET }
    '>'      { RANGLEBRACKET }
    '_'      { WILDCARD }
    '::'     { DOUBLECOLON }
    let      { LET }
    VAR      { VAR $$ }
    OPERATOR { OPERATOR $$ }
    number   { NUMBER $$ }
    boolean  { BOOLEAN $$ }
    string   { STRING $$ }
    char     { CHAR $$ }

%%

Program : Exprs                                    { EProgram $1 }

Exprs : Expr                                       { [$1] }
      | Expr Exprs                                 { $1 : $2 }

Expr : '(' defun VAR '[' Args ']' FormsPlus ')'    { EDestructLetBinding (IdPattern $3) $5 $7 }
     | '(' data con SimpleArgs VConstructors ')'   { unsafePerformIO $ do
                                                         (env, vars) <- foldM (\(env, vars) arg -> do
                                                                                var <- makeVariable
                                                                                return (M.insert arg var env, vars ++ [var]))
                                                                        (M.empty, []) $4
                                                         let dataType = TOper $3 vars
                                                         let constructors' = map (\(EVConstructor cname cargs) -> let cargs' = map (\arg -> case arg of
                                                                                                                                            EVCAVar aname -> readEnv env aname
                                                                                                                                            EVCAOper aname operArgs -> TOper aname $ map (readEnv env) operArgs)
                                                                                                                                  cargs
                                                                                                                                  where readEnv scope n = fromMaybe unitT $ M.lookup n scope
                                                                                                                 in TypeConstructor cname cargs')
                                                                                 $5
                                                         return $ EDataDecl $3 dataType vars constructors' }
     | '(' let VAR FormsPlus ')'                   { EDestructLetBinding (IdPattern $3) [] $4 }
     | Form                                        { $1 }

SimpleArgs : {- empty -}                           { [] }
           | VAR SimpleArgs                        { $1 : $2 }

VConArg : VAR                                      { EVCAVar $1 }
        | con                                      { EVCAOper $1 [] }
        | '(' con SimpleArgs ')'                   { EVCAOper $2 $3 }

VConArgs : VConArg                                 { [$1] }
         | VConArg VConArgs                        { $1 : $2 }

VConstructor : con                                 { EVConstructor $1 [] }
             | '(' con VConArgs ')'                { EVConstructor $2 $3 }

VConstructors : VConstructor                       { [$1] }
              | VConstructor VConstructors         { $1 : $2 }

Args : {- empty -}                                 { [] }
     | VAR Args                                    { (IdPattern $1) : $2 }

Nameds : {- empty -}                               { [] }
       | VAR Nameds                                { (Named $1 Nothing) : $2 }

binding : VAR Form                                 { ELetBinding (IdPattern $1) $2 [] }

bindings : binding                                 { [$1] }
         | binding bindings                        { $1 : $2 }

Form : '(' match Form Cases ')'                    { EPatternMatching $3 $4 }
     | '(' lambda Nameds arrow FormsPlus ')'       { ELambda $3 Nothing $5 }
     | '(' let '[' bindings ']' FormsPlus ')'      { head $ foldr (\(ELetBinding pat def _) body -> [ELetBinding pat def body]) $6 $4 }
     | '(' ListForms ')'                           { $2 }
     | '(' Form FormsPlus ')'                      { foldl (\oper param -> (EApp oper param)) $2 $3 }
     | '[' FormsStar ']'                           { EList $2 }
     | '<' FormsStar '>'                           { ETuple $2 }
     | Atom                                        { $1 }

ListForms : Form '::' Form                         { EApp (EApp (EVar "Cons") $1) $3 }
          | Form '::' ListForms                    { EApp (EApp (EVar "Cons") $1) $3 }

FormsPlus : Form                                   { [$1] }
          | Form FormsPlus                         { $1 : $2 }

FormsStar : {- empty -}                            { [] }
          | Form FormsStar                         { $1 : $2 }

Pattern : '_'                                      { WildcardPattern }
        | VAR                                      { IdPattern $1 }
        | number                                   { NumPattern $1 }
        | boolean                                  { BoolPattern $1 }
        | char                                     { CharPattern $1 }
        | string                                   { StrPattern $1 }
        | con Args                                 { TConPattern $1 $2 }
        | '<' Patterns '>'                         { TuplePattern $2 }
        | '[' ']'                                  { TConPattern "Nil" [] }
        | '[' Patterns ']'                         { foldr (\p t -> TConPattern "Cons" [p, t]) (TConPattern "Nil" []) $2 }
        | ListPatterns                             { $1 }

Patterns : Pattern                                 { [$1] }
         | Pattern Patterns                        { $1 : $2 }

ListPatterns : Pattern '::' Pattern                { TConPattern "Cons" [$1, $3] }
             | Pattern '::' ListPatterns           { TConPattern "Cons" [$1, $3] }

Case : '(' Pattern arrow FormsPlus ')'             { Case $2 $4 }

Cases : Case                                       { [$1] }
      | Case Cases                                 { $1 : $2 }

Atom : boolean                                     { EBool $1 }
     | number                                      { ENum $1 }
     | string                                      { EStr $1 }
     | char                                        { EChar $1 }
     | VAR                                         { EVar $1 }
     | OPERATOR                                    { EVar $1 }
     | con                                         { EVar $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}