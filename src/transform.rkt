#lang racket/base

(provide (all-defined-out))

(require (only-in racket/bool symbol=?))
(require syntax/parse)

(require "cont.rkt")

(define-syntax-class whnf
  [pattern x #:when (whnf? #'x)])

(define-syntax-class binding
  [pattern [var val]])

(define (transform-decl stx)
  (syntax-parse stx
    [((~datum define) var:id body:expr)
     (transform-expr #'body (term-cont (lambda (v) #`(define-value 'var #,v))))]
    [((~datum define) (func:id arg:id ...) body:expr)
     (let-values ([(kvar k) (gen-var-cont)])
       #`(define-value 'func (lambda (arg ... #,kvar) #,(transform-expr #'body k))))]
    [expr:expr
     (transform-expr #'expr (term-cont (lambda (v) #`(print-value #,v))))]
   ))

(define (transform-expr stx k)
  (syntax-parse stx
     [((~datum let*) () expr:expr) (transform-expr #'expr k)]
     [((~datum let*) ([var:id val:expr] rest:binding ...) expr:expr) ; TODO support multiple params
      (let ([converted-expr (transform-expr #'(let* (rest ...) expr) k)])
        (transform-expr #'val (split-cont #'var converted-expr)))]

     [expr:whnf (apply-cont k #'expr)]  ; TODO what if there is a primop here?
     [(func:expr arg:expr ...) (transform-call #'func (syntax->list #'(arg ...)) k)]
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
    (let ([var (syntax-gensym)])
      (cons (lambda (expr) (transform-expr e (split-cont var expr))) var))))

(define (primop? stx)
  (let ([d (syntax-e stx)])
    (and (symbol? d) (symbol=? d '+))))

