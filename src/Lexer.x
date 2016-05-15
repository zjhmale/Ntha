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
$operator = [\+\-\*\/\%\=\>\<]
$chars = [$lower $upper $digit $operator]
$eol = [\n]

tokens :-
       $eol                        ;
       $white+                     ;
       ";;".*                      ; --comments
       -- TODO support multiline comments
       "data"                      { \_ -> DATA }
       "match"                     { \_ -> MATCH }
       "if"                        { \_ -> IF }
       "ƒ" | "fun"                 { \_ -> DEFUN }
       "λ" | "lambda"              { \_ -> LAMBDA }
       "⇒" | "=>" | "→" | "->"    { \_ -> ARROW }
       "["                         { \_ -> LBRACKET }
       "]"                         { \_ -> RBRACKET }
       "("                         { \_ -> LPAREN }
       ")"                         { \_ -> RPAREN }
       "{"                         { \_ -> LBRACE }
       "}"                         { \_ -> RBRACE }
       "_"                         { \_ -> WILDCARD }
       "."                         { \_ -> DOT }
       ":" $chars+                 { \s -> KEYWORD (tail s) }
       ":"                         { \_ -> COLON }
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
           | IF
           | ARROW
           | LBRACKET
           | RBRACKET
           | LPAREN
           | RPAREN
           | LBRACE
           | RBRACE
           | WILDCARD
           | DOT
           | COLON
           | DOUBLECOLON
           | VAR EName
           | CON EName -- constructor
           | LET
           | KEYWORD String
           | OPERATOR String
           | BOOLEAN Bool
           | NUMBER Int
           | STRING String
           | CHAR Char
           deriving(Eq, Show)

scanTokens = alexScanTokens
}