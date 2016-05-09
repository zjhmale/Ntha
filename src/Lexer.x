{
module Lexer where
import Ast(EName)
}

%wrapper "basic"

$capital = [A-Z]
$letter = [a-zA-Z]
$eol = [\n]

tokens :-
       $eol         ;
       $white+      ;
       ";;".*       ; --comments
       "{-".*"-}"   ; --multicomments
       "data"       { \_ -> DATA }
       "match"      { \_ -> MATCH }
       "ƒ"          { \_ -> DEFUN }
       "⇒"          { \_ -> ARROW }
       "["          { \_ -> LBRACKET }
       "]"          { \_ -> RBRACKET }
       "("          { \_ -> LPAREN }
       ")"          { \_ -> RPAREN }
       $capital $letter+ { \s -> VCON s }
       $letter+     { \s -> VAR s }

{
data Token = DATA
           | MATCH
           | DEFUN
           | ARROW
           | LBRACKET
           | RBRACKET
           | LPAREN
           | RPAREN
           | VAR EName
           | VCON EName -- value constructor
           deriving(Eq, Show)

scanTokens = alexScanTokens
}