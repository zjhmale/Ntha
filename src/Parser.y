{
module Parser where

import Ast
import Type
import Lexer
import State
import Control.Monad
import Data.IORef
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
    type     { TYPE }
    defun    { DEFUN }
    lambda   { LAMBDA }
    monad    { MONAD }
    do       { DO }
    return   { RETURN }
    if       { IF }
    rarrow   { RARROW }
    larrow   { LARROW }
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
                                                           let constructors' = map (\(EVConstructor cname cargs) -> let cargs' = map getType
                                                                                                                                     cargs
                                                                                                                                     where readEnv scope n = fromMaybe unitT $ M.lookup n scope
                                                                                                                                           getType arg = case arg of
                                                                                                                                                           EVCAVar aname -> readEnv env aname
                                                                                                                                                           EVCAOper aname operArgs -> TOper aname $ map (readEnv env) operArgs
                                                                                                                                                           EVCAList arg' -> TOper "List" [getType arg']
                                                                                                                                                           EVCATuple args -> TOper "*" (map getType args)
                                                                                                                    in TypeConstructor cname cargs')
                                                                                   $5
                                                           return $ EDataDecl $3 dataType vars constructors' }
     | '(' let Pattern FormsPlus ')'                 { EDestructLetBinding $3 [] $4 }
     | '(' type con VConArg ')'                      { unsafePerformIO $ do
                                                        $4 `seq` modifyIORef aliasMap $ M.insert $3 $4
                                                        return EUnit }
     | '(' monad con Form ')'                        { unsafePerformIO $ do
                                                        $4 `seq` modifyIORef monadMap $ M.insert $3 $4
                                                        return $ EDestructLetBinding (IdPattern $3) [] [$4] }
     | Form                                          { $1 }

SimpleArgs : {- empty -}                             { [] }
           | VAR SimpleArgs                          { $1 : $2 }

VConArg : VAR                                        { EVCAVar $1 }
        | con                                        { unsafePerformIO $ do
                                                        alias <- readIORef aliasMap
                                                        case M.lookup $1 alias of
                                                          Just vconarg -> return vconarg
                                                          Nothing -> if $1 == "String"
                                                                     then return $ EVCAList (EVCAOper "Char" []) -- special case for String pattern
                                                                     else return $ EVCAOper $1 [] }
        | '(' con SimpleArgs ')'                     { EVCAOper $2 $3 }
        -- TODO more specs here
        | '[' VConArg ']'                            { EVCAList $2 }
        | '(' TupleVConArgs ')'                      { EVCATuple $2 }

TupleVConArgs : VConArg '.' VConArg                  { [$1, $3] }
              | VConArgs '.' VConArg                 { $1 ++ [$3] }

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

binding : Pattern Form                               { ELetBinding $1 $2 [] }

bindings : binding                                   { [$1] }
         | binding bindings                          { $1 : $2 }

bind : Form                                          { Single $1 }
     | '(' return Form ')'                           { Return $3 }
     | '(' VAR larrow Form ')'                       { Bind $2 $4 }

binds : bind                                         { [$1] }
      | bind binds                                   { $1 : $2 }

Form : '(' match Form Cases ')'                      { EPatternMatching $3 $4 }
     | '(' lambda Nameds rarrow FormsPlus ')'        { ELambda $3 Nothing $5 }
     | '(' lambda Nameds ':' con rarrow FormsPlus ')'{ ELambda $3 (Just (TOper $5 [])) $7 }
     | '(' let '[' bindings ']' FormsPlus ')'        { head $ foldr (\(ELetBinding pat def _) body -> [ELetBinding pat def body]) $6 $4 }
     | '(' if Form Form Form ')'                     { EIf $3 [$4] [$5] }
     | '(' do con binds ')'                          { unsafePerformIO $ do
                                                        monads <- readIORef monadMap
                                                        return $ case M.lookup $3 monads of
                                                                   Just (ERecord pairs) -> case M.lookup "return" pairs of
                                                                                             Just rtn -> case M.lookup ">>=" pairs of
                                                                                                           Just bind -> foldr (\b next -> case next of
                                                                                                                                            EUnit -> case b of
                                                                                                                                                       Bind n e -> error "illegal do expression"
                                                                                                                                                       Return e -> EApp rtn e
                                                                                                                                                       Single e -> e
                                                                                                                                            _ -> case b of
                                                                                                                                                    Bind n e -> EApp (EApp bind e) (ELambda [Named n Nothing] Nothing [next])
                                                                                                                                                    Return e -> EApp rtn e
                                                                                                                                                    Single e -> e)
                                                                                                                              EUnit $4
                                                                                                           Nothing -> error $ "bind function is not defined for " ++ $3 ++ " monad"
                                                                                             Nothing -> error $ "return function is not defined for " ++ $3 ++ " monad"
                                                                   _ -> error $ $3 ++ " monad is not defined" }
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
        | '(' ListDestructPats ')'                   { $2 }

Patterns : Pattern                                   { [$1] }
         | Pattern Patterns                          { $1 : $2 }

TuplePatterns : Pattern '.' Pattern                  { [$1, $3] }
              | TuplePatterns '.' Pattern            { $1 ++ [$3] }

ListPatterns : Pattern '::' Pattern                  { TConPattern "Cons" [$1, $3] }
             | Pattern '::' ListPatterns             { TConPattern "Cons" [$1, $3] }

ListDestructPats : Pattern '::' Pattern              { TConPattern "Cons" [$1, TConPattern "Cons" [$3, TConPattern "Nil" []]] }
                 | Pattern '::' ListDestructPats     { TConPattern "Cons" [$1, $3] }

Case : '(' Pattern rarrow FormsPlus ')'              { Case $2 $4 }

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
aliasMap :: IORef (M.Map String EVConArg)
aliasMap = createState M.empty

monadMap :: IORef (M.Map String Expr)
monadMap = createState M.empty

parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}