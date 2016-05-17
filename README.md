# Ntha Programming Language

[![Build Status](https://travis-ci.org/zjhmale/Ntha.svg?branch=master)](https://travis-ci.org/zjhmale/Ntha)
[![zjhmale](https://img.shields.io/badge/author-%40zjhmale-blue.svg)](https://github.com/zjhmale)
[![Haskell](https://img.shields.io/badge/language-haskell-red.svg)](https://en.wikipedia.org/wiki/Haskell_(programming_language))

a tiny statically typed functional programming language.

## Features

* Global type inference with optional type annotations.
* Lisp flavored syntax with Haskell like semantic inside.
* Support basic types: Integer, Character, String, Boolean, Tuple, List and Record.
* Support unicode keywords.
* Support destructuring.
* ADTs and pattern matching.
* Support pattern matching on function parameters.
* Lambdas and curried function by default.
* Global and Local let binding.
* Recursive functions.
* If-then-else control flow.
* Type alias.
* Do notation.

## Future Works

* Module system.
* error propagation (try / catch).
* JIT backend.
* Type-classes.
* Rank-N types.
* Dependent types.
* Fully type checked lisp like macros.
* TCO.

## Screenshots

![cleantha](./screenshots.gif)

## Example

```Clojure
(type Name String)

(data Op Add Sub Mul Div)

(data Expr
  (Num Number)
  (Var Name)
  (Let [Char] Expr Expr)
  (Binop Op (Expr . Expr)))

(let op-map {:add +
             :sub -
             :mul *
             :div /})

(ƒ do-eval [fn (v1 . v2)]
  (Just (+ v1 v2)))

(let eval-op
  (λ op v1 v2 ⇒
    (match (v1 . v2)
      (((Just v1) . (Just v2)) ⇒
        (match op
          (Add ⇒ (do-eval (:add op-map) (v1 . v2)))
          (Sub ⇒ (do-eval (:sub op-map) (v1 . v2)))
          (Mul ⇒ (do-eval (:mul op-map) (v1 . v2)))
          (Div ⇒ (do-eval (:div op-map) (v1 . v2)))))
      (_ ⇒ Nothing))))

(ƒ eval [env expr]
  (match expr
    ((Num i) ⇒ (Just i))
    ((Var x) ⇒ (lookup x env))
    ((Let x e1 in-e2) ⇒ (do Maybe
                          (v ← (eval env e1))
                          (eval ((x . v) :: env) in-e2)))
    ((Binop op (e1 . e2)) => (let [v1 (eval env e1)
                                   v2 (eval env e2)]
                               (eval-op op v1 v2)))))

(match (eval [] (Let "x" (Num 1) (Binop Add ((Var "x") . (Var "x")))))
  ((Just num) ⇒ (print (int2str num)))
  (Nothing ⇒ (error "oops")))
```

## License

[![license BSD](https://img.shields.io/badge/license-BSD-orange.svg)](https://en.wikipedia.org/wiki/BSD_licenses)
