{
module Lexer where
import Ast(EName)
}

%wrapper "basic"

$letter = [a-zA-Z]
$eol    = [\n]

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
           deriving(Eq, Show)

scanTokens = alexScanTokens
}