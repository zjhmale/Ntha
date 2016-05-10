{
module Parser where

import Ast
import Lexer
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
    let      { LET }
    define   { DEFINE }
    VAR      { VAR $$ }
    OPERATOR { OPERATOR $$ }
    number   { NUMBER $$ }
    boolean  { BOOLEAN $$ }

%%

Expr : '(' defun VAR '[' Args ']' Forms ')'        { EDestructLetBinding (IdPattern $3) $5 $7 }
     | '(' data con SimpleArgs VConstructors ')'   { mkDataDeclExpr (ETConstructor $3 $4 $5) }
     | '(' define VAR Forms ')'                    { EDestructLetBinding (IdPattern $3) [] $4 }
     | Form                                        { $1 }

SimpleArgs : {- empty -}                           { [] }
           | VAR SimpleArgs                        { $1 : $2 }

VConArg : VAR                                      { EVCAVar $1 }
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

binding : VAR Form                                 { ELetBinding (IdPattern $1) $2 [EUnit] }

bindings : binding                                 { [$1] }
         | binding bindings                        { $1 : $2 }

Form : '(' match VAR Cases ')'                     { EPatternMatching (EVar $3) $4 }
     | '(' lambda Nameds arrow Forms ')'           { ELambda $3 Nothing $5 }
     -- | '(' let '[' bindings ']' Forms ')'          {  }
     | '(' Form Form Form ')'                      { EApp (EApp $2 $3) $4 }
     | '[' Atoms ']'                               { EList $2 }
     | Atom                                        { $1 }

Forms : Form                                       { [$1] }
      | Form Forms                                 { $1 : $2 }

Pattern : con Args                                 { TConPattern $1 $2 }

Case : '(' Pattern arrow Forms ')'                 { Case $2 $4 }

Cases : Case                                       { [$1] }
      | Case Cases                                 { $1 : $2 }

Atom : boolean                                     { EBool $1 }
     | number                                      { ENum $1 }
     | VAR                                         { EVar $1 }
     | OPERATOR                                    { EVar $1 }
     | con                                         { EVar $1 }

Atoms : {- empty -}                                { [] }
      | Atom Atoms                                 { $1 : $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}