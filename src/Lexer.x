{
module Lexer where
import Ast (EName)
import Data.Char (toUpper)
}

%wrapper "basic"

$capital = [A-Z]
$letter = [a-zA-Z]
$eol = [\n]
$operator = [\+\-\*\/]
$digit = 0-9

tokens :-
       $eol              ;
       $white+           ;
       ";;".*            ; --comments
       "{-".*"-}"        ; --multicomments
       "data"            { \_ -> DATA }
       "match"           { \_ -> MATCH }
       "ƒ"               { \_ -> DEFUN }
       "λ"               { \_ -> LAMBDA }
       "⇒"               { \_ -> ARROW }
       "["               { \_ -> LBRACKET }
       "]"               { \_ -> RBRACKET }
       "("               { \_ -> LPAREN }
       ")"               { \_ -> RPAREN }
       "let"             { \_ -> LET }
       "true" | "false"  { \s -> BOOLEAN (read ([toUpper (s!!0)] ++ tail s)) }
       $capital $letter+ { \s -> CON s }
       $letter+          { \s -> VAR s }
       $operator         { \s -> OPERATOR s }
       $digit+           { \s -> NUMBER (read s) }

{
data Token = DATA
           | MATCH
           | DEFUN
           | LAMBDA
           | ARROW
           | LBRACKET
           | RBRACKET
           | LPAREN
           | RPAREN
           | VAR EName
           | CON EName -- constructor
           | LET
           | OPERATOR String
           | BOOLEAN Bool
           | NUMBER Int
           deriving(Eq, Show)

scanTokens = alexScanTokens
}