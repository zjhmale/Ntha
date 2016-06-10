{
module Lexer where
import Ast (EName)
import Data.Char (toUpper)
}

%wrapper "basic"

$upper = [A-Z]
$lower = [a-z]
$greek = [α-ω]
$digit = [0-9]
$operator = [\+\-\*\/\%\=\>\<\∧\∨\¬\?\'\~\!]
$chars = [$lower $upper $digit $operator $greek]
$eol = [\n]

tokens :-
       $eol                        ;
       $white+                     ;
       ";;".*                      ; --comments
       -- TODO support multiline comments
       "data"                      { \_ -> DATA }
       "match"                     { \_ -> MATCH }
       "begin"                     { \_ -> BEGIN }
       "type"                      { \_ -> TYPE }
       "if"                        { \_ -> IF }
       "cond"                      { \_ -> COND }
       "else"                      { \_ -> ELSE }
       "monad"                     { \_ -> MONAD }
       "do"                        { \_ -> DO }
       "return"                    { \_ -> RETURN }
       "ƒ" | "fun"                 { \_ -> DEFUN }
       "λ" | "lambda"              { \_ -> LAMBDA }
       "⇒" | "=>" | "→" | "->"     { \_ -> RARROW }
       "⇐" | "<=" | "←" | "<-"     { \_ -> LARROW }
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
       "|"                         { \_ -> BAR }
       "let"                       { \_ -> LET }
       "Z"                         { \_ -> NUMBERT }
       "B"                         { \_ -> BOOLT }
       "C"                         { \_ -> CHART }
       "S"                         { \_ -> STRT }
       "×"                         { \_ -> PRODUCT }
       "true" | "false"            { \s -> BOOLEAN (read ([toUpper (s!!0)] ++ tail s)) }
       $upper $chars*              { \s -> CON s }
       $lower $chars*              { \s -> VAR s }
       $greek                      { \s -> TVAR (s!!0) }
       \"[^\"]*\"                  { \s -> STRING ((tail . init) s) }
       '[^'\"]{1}'                 { \s -> CHAR ((head . tail . init) s) }
       $operator | "≠" | "≤" | "≥" { \s -> OPERATOR s }
       $digit+                     { \s -> NUMBER (read s) }
       "-" $digit+                 { \s -> NUMBER (read s) }

{
data Token = DATA
           | MATCH
           | BEGIN
           | TYPE
           | DEFUN
           | LAMBDA
           | MONAD
           | DO
           | RETURN
           | IF
           | COND
           | ELSE
           | RARROW
           | LARROW
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
           | BAR
           | VAR EName
           | TVAR Char
           | CON EName -- constructor names or uppercase symbols
           | LET
           | NUMBERT
           | BOOLT
           | CHART
           | STRT
           | PRODUCT
           | KEYWORD String
           | OPERATOR String
           | BOOLEAN Bool
           | NUMBER Int
           | STRING String
           | CHAR Char
           deriving(Eq, Show)

scanTokens = alexScanTokens
}