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
    if       { IF }
    arrow    { ARROW }
    con      { CON $$ }
    '['      { LBRACKET }
    ']'      { RBRACKET }
    '('      { LPAREN }
    ')'      { RPAREN }
    '{'      { LBRACE }
    '}'      { RBRACE }
    '_'      { WILDCARD }
    '.'      { DOT }
    ':'      { COLON }
    '::'     { DOUBLECOLON }
    let      { LET }
    keyword  { KEYWORD $$ }
    VAR      { VAR $$ }
    OPERATOR { OPERATOR $$ }
    number   { NUMBER $$ }
    boolean  { BOOLEAN $$ }
    string   { STRING $$ }
    char     { CHAR $$ }

%%

Program : Exprs                                      { EProgram $1 }

Exprs : Expr                                         { [$1] }
      | Expr Exprs                                   { $1 : $2 }

Expr : '(' defun VAR '[' Args ']' FormsPlus ')'      { EDestructLetBinding (IdPattern $3) $5 $7 }
     | '(' data con SimpleArgs VConstructors ')'     { unsafePerformIO $ do
                                                           (env, vars) <- foldM (\(env, vars) arg -> do
                                                                                  var <- makeVariable
                                                                                  return (M.insert arg var env, vars ++ [var]))
                                                                                (M.empty, []) $4
                                                           let dataType = TOper $3 vars
                                                           let constructors' = map (\(EVConstructor cname cargs) -> let cargs' = map (\arg -> case arg of
                                                                                                                                                EVCAVar aname -> readEnv env aname
                                                                                                                                                EVCAOper aname operArgs -> TOper aname $ map (readEnv env) operArgs
                                                                                                                                                EVCAList t -> TOper "List" [t])
                                                                                                                                      cargs
                                                                                                                                      where readEnv scope n = fromMaybe unitT $ M.lookup n scope
                                                                                                                    in TypeConstructor cname cargs')
                                                                                   $5
                                                           return $ EDataDecl $3 dataType vars constructors' }
     | '(' let VAR FormsPlus ')'                     { EDestructLetBinding (IdPattern $3) [] $4 }
     | Form                                          { $1 }

SimpleArgs : {- empty -}                             { [] }
           | VAR SimpleArgs                          { $1 : $2 }

VConArg : VAR                                        { EVCAVar $1 }
        | con                                        { EVCAOper $1 [] }
        | '(' con SimpleArgs ')'                     { EVCAOper $2 $3 }
        -- TODO should be more generic for list and tuple pattern, for now just support basic type operator like Number Char etc.
        | '[' con ']'                                { EVCAList (TOper $2 []) }

VConArgs : VConArg                                   { [$1] }
         | VConArg VConArgs                          { $1 : $2 }

VConstructor : con                                   { EVConstructor $1 [] }
             | '(' con VConArgs ')'                  { EVConstructor $2 $3 }

VConstructors : VConstructor                         { [$1] }
              | VConstructor VConstructors           { $1 : $2 }

Args : {- empty -}                                   { [] }
     | Pattern Args                                  { $1 : $2 }

Nameds : {- empty -}                                 { [] }
       | VAR Nameds                                  { (Named $1 Nothing) : $2 }
       | '(' VAR ':' con ')' Nameds                  { (Named $2 (Just (TOper $4 []))) : $6 }

binding : VAR Form                                   { ELetBinding (IdPattern $1) $2 [] }

bindings : binding                                   { [$1] }
         | binding bindings                          { $1 : $2 }

Form : '(' match Form Cases ')'                      { EPatternMatching $3 $4 }
     | '(' lambda Nameds arrow FormsPlus ')'         { ELambda $3 Nothing $5 }
     | '(' lambda Nameds ':' con arrow FormsPlus ')' { ELambda $3 (Just (TOper $5 [])) $7 }
     | '(' let '[' bindings ']' FormsPlus ')'        { head $ foldr (\(ELetBinding pat def _) body -> [ELetBinding pat def body]) $6 $4 }
     | '(' if Form Form Form ')'                     { EIf $3 [$4] [$5] }
     | '(' ListForms ')'                             { $2 }
     | '(' TupleFroms ')'                            { ETuple $2 }
     | '(' Form FormsPlus ')'                        { foldl (\oper param -> (EApp oper param)) $2 $3 }
     | '(' OPERATOR FormsPlus ')'                    { case $3 of
                                                         a:[] -> EApp (EVar $2) a
                                                         a:b:[] -> EApp (EApp (EVar $2) a) b
                                                         a:b:xs -> foldl (\oper param -> (EApp (EApp (EVar $2) oper) param)) (EApp (EApp (EVar $2) a) b) xs }
     | '[' FormsStar ']'                             { EList $2 }
     | '{' RecordForms '}'                           { ERecord $2 }
     | '(' keyword Form ')'                          { EAccessor $3 $2 }
     | Atom                                          { $1 }

RecordForms : keyword Form                           { M.singleton $1 $2 }
            | RecordForms keyword Form               { M.insert $2 $3 $1 }

ListForms : Form '::' Form                           { EApp (EApp (EVar "Cons") $1) $3 }
          | Form '::' ListForms                      { EApp (EApp (EVar "Cons") $1) $3 }

TupleFroms : Form '.' Form                           { [$1, $3] }
           | TupleFroms '.' Form                     { $1 ++ [$3] }

FormsPlus : Form                                     { [$1] }
          | Form FormsPlus                           { $1 : $2 }

FormsStar : {- empty -}                              { [] }
          | Form FormsStar                           { $1 : $2 }

Pattern : '_'                                        { WildcardPattern }
        | VAR                                        { IdPattern $1 }
        | number                                     { NumPattern $1 }
        | boolean                                    { BoolPattern $1 }
        | char                                       { CharPattern $1 }
        | string                                     { foldr (\p t -> TConPattern "Cons" [p, t]) (TConPattern "Nil" []) (map CharPattern $1) }
        | con                                        { TConPattern $1 [] }
        | '(' con Args ')'                           { TConPattern $2 $3 }
        | '(' TuplePatterns ')'                      { TuplePattern $2 }
        | '[' ']'                                    { TConPattern "Nil" [] }
        | '[' Patterns ']'                           { foldr (\p t -> TConPattern "Cons" [p, t]) (TConPattern "Nil" []) $2 }
        | ListPatterns                               { $1 }

Patterns : Pattern                                   { [$1] }
         | Pattern Patterns                          { $1 : $2 }

TuplePatterns : Pattern '.' Pattern                  { [$1, $3] }
              | TuplePatterns '.' Pattern            { $1 ++ [$3] }

ListPatterns : Pattern '::' Pattern                  { TConPattern "Cons" [$1, $3] }
             | Pattern '::' ListPatterns             { TConPattern "Cons" [$1, $3] }

Case : '(' Pattern arrow FormsPlus ')'               { Case $2 $4 }

Cases : Case                                         { [$1] }
      | Case Cases                                   { $1 : $2 }

Atom : boolean                                       { EBool $1 }
     | number                                        { ENum $1 }
     | string                                        { EStr $1 }
     | char                                          { EChar $1 }
     | VAR                                           { EVar $1 }
     | OPERATOR                                      { EVar $1 }
     | con                                           { EVar $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}