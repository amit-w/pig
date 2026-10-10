#lang racket/base

(require racket/contract)
(require racket/match)

(provide (all-defined-out))

(define (whnf? expr)
  (or (number? expr)
      (var? expr)))
(define (var? expr) (symbol? expr))
(define formals/c (listof var?))
(define primop/c '+)

(define atomic-form/c
  (or/c number? var?))
(define quoted-name/c (list/c 'quote symbol?))
(define expr/c
  (rename-contract
    (or/c atomic-form/c (recursive-contract compound-form/c))
    'expression))
(define binding/c
  (list/c var? expr/c))

(define compound-form/c
  (cons/dc
    [head symbol-interned?]
    [tail (head)
      (match head
          ['lambda       (list/c formals/c expr/c)]
          ['let          (list/c (listof binding/c) expr/c)]
          ['define-value (list/c quoted-name/c whnf?)]
          ['print-value  (list/c expr/c)]
          ['#%app        (cons/c var? (listof expr/c))]
          ['#%primop     (cons/c primop/c (listof whnf?))])]))

