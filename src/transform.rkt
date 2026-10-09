#lang racket/base

(provide (all-defined-out))

(require (only-in racket/bool symbol=?)
         (only-in racket/match match))

(require "cont.rkt")

(define (transform-decl decl)
  (match decl
    [`(define ,var ,body)
     #:when (symbol? var)
     (transform-expr body (term-cont (lambda (v) `(define-value ',var ,v))))]
    [`(define (,func ,@args) ,body)
     (let-values ([(kvar k) (gen-var-cont)])
       `(define-value ',func (lambda (,@args ,kvar) ,(transform-expr body k))))]
    [expr
     (transform-expr expr (term-cont (lambda (v) `(print-value ,v))))]
   ))

(define (transform-expr expr k)
  (match expr
     [`(let* (,@bindings) ,expr)
      (foldr apply-binding (transform-expr expr k) bindings)]

     [expr #:when (whnf? expr) (apply-cont k expr)]  ; TODO what if there is a primop here?
     [`(,func ,@args) (transform-call func args k)]
  ))

(define (apply-binding binding expr)
  (match binding
    [(list var val) (transform-expr val (split-cont var expr))]))

(define (transform-call func args k)
  #|Assumptions:
      - func is whnf
      - we know how to handle each arg in args|#
  (let* ([xs (map split args)]
         [new-args (map cdr xs)]
         [expr (build-call-base-expr func new-args k)])
    (foldr (lambda (pair e) ((car pair) e)) expr xs)))

(define (build-call-base-expr func args k)
  (if (primop? func)
    (let ([expr `(#%primop ,func ,@args)]) (use-let expr k))
    `(#%app ,func ,@args ,(reify-cont k))))

(define (split e)
  (if (whnf? e)
    (cons (lambda (expr) expr) e)
    (let ([var (gensym)])
      (cons (lambda (expr) (transform-expr e (split-cont var expr))) var))))

(define (primop? id)
  (and (symbol? id) (symbol=? id '+)))

