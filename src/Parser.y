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
    arrow    { ARROW }
    '[' { LBRACKET }
    ']' { RBRACKET }
    '('      { LPAREN }
    ')'      { RPAREN }
    VAR      { VAR $$ }

%%

Expr : '(' defun VAR '[' Args ']' Forms ')'        { EDestructLetBinding (IdPattern $3) $5 $7 }
     | Form                                        { $1 }

Args : {- empty -}                                 { [] }
     | Args VAR                                    { (IdPattern $2) : $1 }

Form : '(' VAR VAR VAR ')'                         { EApp (EApp (EVar $2) (EVar $3)) (EVar $4) }

Forms : Form                                       { [$1] }
      | Forms Form                                 { $2 : $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}