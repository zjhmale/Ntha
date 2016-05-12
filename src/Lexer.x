{
module Lexer where
import Ast (EName)
import Data.Char (toUpper)
}

%wrapper "basic"

$upper = [A-Z]
$lower = [a-z]
$digit = [0-9]
$letter = [$lower $upper]
$chars = [$lower $upper $digit]
$eol = [\n]
$operator = [\+\-\*\/\%\=\>\<]

tokens :-
       $eol                        ;
       $white+                     ;
       ";;".*                      ; --comments
       "{-".*"-}"                  ; --multicomments
       "data"                      { \_ -> DATA }
       "match"                     { \_ -> MATCH }
       "ƒ" | "fun"                 { \_ -> DEFUN }
       "λ" | "lambda"              { \_ -> LAMBDA }
       "⇒" | "=>"                  { \_ -> ARROW }
       "["                         { \_ -> LBRACKET }
       "]"                         { \_ -> RBRACKET }
       "("                         { \_ -> LPAREN }
       ")"                         { \_ -> RPAREN }
       "_"                         { \_ -> WILDCARD }
       "."                         { \_ -> DOT }
       "∷" | "::"                  { \_ -> DOUBLECOLON }
       "let"                       { \_ -> LET }
       "true" | "false"            { \s -> BOOLEAN (read ([toUpper (s!!0)] ++ tail s)) }
       $upper $chars*              { \s -> CON s }
       $lower $chars*              { \s -> VAR s }
       \"[^\"]*\"                  { \s -> STRING ((tail . init) s) }
       '[^'\"]{1}'                 { \s -> CHAR ((head . tail . init) s) }
       $operator | "≠" | "≤" | "≥" { \s -> OPERATOR s }
       $digit+                     { \s -> NUMBER (read s) }

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
           | WILDCARD
           | DOT
           | DOUBLECOLON
           | VAR EName
           | CON EName -- constructor
           | LET
           | OPERATOR String
           | BOOLEAN Bool
           | NUMBER Int
           | STRING String
           | CHAR Char
           deriving(Eq, Show)

scanTokens = alexScanTokens
}