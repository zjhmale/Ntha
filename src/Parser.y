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
    VAR      { VAR $$ }

%%

Expr : '(' defun VAR '[' Args ']' Forms ')'        { EDestructLetBinding (IdPattern $3) $5 $7 }
     | '(' data con SimpleArgs VConstructors ')'   { mkDataDeclExpr (ETConstructor $3 $4 $5) }
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

Form : '(' match VAR Cases ')'                     { EPatternMatching (EVar $3) $4 }
     | '(' lambda Nameds arrow Forms ')'           { ELambda $3 Nothing $5 }
     | '(' VAR VAR VAR ')'                         { EApp (EApp (EVar $2) (EVar $3)) (EVar $4) }

Forms : Form                                       { [$1] }
      | Form Forms                                 { $1 : $2 }

Pattern : con Args                                 { TConPattern $1 $2 }

Case : '(' Pattern arrow Forms ')'                 { Case $2 $4 }

Cases : Case                                       { [$1] }
      | Case Cases                                 { $1 : $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}