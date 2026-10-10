#lang racket/base

(require racket/contract)
(require racket/match)

(provide (all-defined-out))

(define (var? expr) (symbol? expr))
(define formals/c (listof var?))
(define whnf/c
  (or/c integer?
        var?
        (list/c 'lambda (listof formals/c) (recursive-contract expr/c))))
(define primop/c '+)

(define atomic-form/c
  (or/c integer? var?))
(define quoted-name/c (list/c 'quote symbol?))
(define expr/c
  (rename-contract
    (or/c atomic-form/c (recursive-contract compound-form/c))
    'expression))
(define simple-expr/c
  (rename-contract
   (or/c atomic-form/c
         (recursive-contract simple-compound-form/c))
   'simple-expression))
(define binding/c
  (list/c var? simple-expr/c))

(define simple-compound-form/c
  (cons/dc
    [head symbol-interned?]
    [tail (head)
      (match head
        ['lambda       (list/c formals/c expr/c)]
        ['#%primop     (cons/c primop/c (listof whnf/c))])]))

(define compound-form/c
  (cons/dc
    [head symbol-interned?]
    [tail (head)
      (match head
        ['lambda       (list/c formals/c expr/c)]
        ['let          (list/c (listof binding/c) expr/c)]
        ['define-value (list/c quoted-name/c whnf/c)]
        ['print-value  (list/c whnf/c)]
        ['#%app        (cons/c var? (listof whnf/c))]
        ['#%primop     (cons/c primop/c (listof whnf/c))])]))

