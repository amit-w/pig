#lang racket/base

(provide (all-defined-out))

(require (only-in racket/bool symbol=?))

(require "cont.rkt")

(define (transform-decl stx)
  (syntax-case stx (define)
    [(define var body) (identifier? #'var)
     (transform-expr #'body (term-cont (lambda (v) #`(define-value var #,v))))]
    [(define (func arg ...) body)
     (let-values ([(kvar k) (gen-var-cont)])
       #`(define-value func (lambda (arg ... #,kvar) #,(transform-expr #'body k))))]
    [expr
     (transform-expr #'expr (term-cont (lambda (v) #`(print-value #,v))))]
   ))

(define (transform-expr stx k)
  (syntax-case stx (let)
     [(let ([var val]) expr) ; TODO support multiple params
      (let ([converted-expr (transform-expr #'expr k)])
        (transform-expr #'val (split-cont #'var converted-expr)))]

     [expr (whnf? #'expr) (apply-cont k #'expr)]  ; TODO what if there is a primop here?
     [(func arg ...) (transform-call #'func (syntax->list #'(arg ...)) k)]
  ))

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
    (let ([expr #`(#%primop #,func #,@args)]) (use-let expr k))
    #`(#%app #,func #,@args #,(reify-cont k))))

(define (split e)
  (if (whnf? e)
    (cons (lambda (expr) expr) e)
    (let ([var (gensym)])
      (cons (lambda (expr) (transform-expr e (split-cont var expr))) var))))

(define (primop? stx)
  (let ([d (syntax-e stx)])
    (and (symbol? d) (symbol=? d '+))))

