#lang typed/racket/base

(require racket/match)

(provide (all-defined-out))

(define-type SimpleExpr (U Symbol Integer primop-call fn cont))
(define-type Expr (U Symbol Integer define-value print-value fn call let1 primop-call cont enter-cont))
(define-type Primop '+)

(struct define-value ([var : Symbol] [val : SimpleExpr]))
(struct print-value ([val : SimpleExpr]))

(struct fn ([kvar : Symbol] [vars : (Listof Symbol)] [body : Expr]))
(struct call ([func : Symbol] [k : SimpleExpr] [args : (Listof SimpleExpr)]))

(struct let1 ([var : Symbol] [val : SimpleExpr] [body : Expr]))
(struct primop-call ([primop : Primop] [args : (Listof SimpleExpr)]))

(struct cont ([var : Symbol] [body : Expr]))
(struct enter-cont ([kvar : Symbol] [arg : SimpleExpr]))

(define (is-whnf? cps-expr) (or (number? cps-expr) (symbol? cps-expr)))
(define (to-s-exp [cps-expr : Expr]) : Any
  (match cps-expr
    [(define-value var val)    `(define-value ',var ,(to-s-exp val))]
    [(print-value val)         `(print-value ,(to-s-exp val))]
    [(fn kvar vars body)       `(lambda (,@vars ,kvar) ,(to-s-exp body))]
    [(call func k args)        `(,(to-s-exp func) ,@args ,(to-s-exp k))]
    [(let1 var val body)       `(let ([,var ,(to-s-exp val)]) ,(to-s-exp body))]
    [(primop-call primop args) `(,primop ,@args)]
    [(cont var body)           `(lambda (,var) ,(to-s-exp body))]
    [(enter-cont kvar arg)     `(,(to-s-exp kvar) ,arg)]
    [_ #:when (is-whnf? cps-expr) cps-expr]
  ))

