{
module Lexer where
import Ast(EName)
}

%wrapper "basic"

$capital = [A-Z]
$letter = [a-zA-Z]
$eol = [\n]

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
       "≡"               { \_ -> DEFINE }
       $capital $letter+ { \s -> CON s }
       $letter+          { \s -> VAR s }

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
           | DEFINE
           deriving(Eq, Show)

scanTokens = alexScanTokens
}