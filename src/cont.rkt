#lang racket/base

(provide (all-defined-out))

(require (only-in racket/match match))

(define (term-cont func) (cons 'term func))
(define (split-cont var expr) (cons 'split (cons var expr)))
(define (var-cont var) (cons 'var var))
(define (gen-split-cont expr)
  (let* ([var (gensym)]) (values var (split-cont var expr))))
(define (gen-var-cont)
  (let* ([var (gensym)]) (values var (var-cont var))))

(define (use-let e k)
  (match k
    [(cons 'term f) (term-cont-use-let e f)]
    [(cons 'split (cons v ke)) (split-cont-use-let e v ke)]
    [(cons 'var v) (var-cont-use-let e v)]))

(define (apply-cont k e)
  (match k
    [(cons 'term f) (apply-term-cont f e)]
    [(cons 'split (cons v ke)) (apply-split-cont v ke e)]
    [(cons 'var v) `(#%app ,v ,e)]))

(define (reify-cont k)
  (match k
    [(cons 'term f) (reify-term-cont f)]
    [(cons 'split (cons v ke)) (reify-split-cont v ke)]
    [(cons 'var v) v]))

; private (TODO remove comment & enforce with provide)
(define (apply-term-cont f e)
  (cond
   [(whnf? e) (f e)]
   [(simple-expr? e) (term-cont-use-let e f)]
   [else `(,e ,(reify-term-cont f))]))

(define (apply-split-cont v ke e)
  (cond
   [(simple-expr? e) (split-cont-use-let e v ke)]
   [else `(,e ,(reify-split-cont v ke))]))

(define (reify-term-cont f)
  (let ([var (gensym)])
    `(lambda (,var) ,(f var))))

(define (reify-split-cont v ke)
  `(lambda (,v) ,ke))

(define (term-cont-use-let e f)
  (let ([var (gensym)])
    `(let ([,var ,e]) ,(f var))))

(define (split-cont-use-let e v ke)
  `(let ([,v ,e]) ,ke))

(define (var-cont-use-let e kvar)
  (let ([v (gensym)])
    `(let ([,v ,e]) (#%app ,kvar ,v))))

; find another place for this
(define (whnf? expr)
  (or (number? expr)
      (symbol? expr)))
(define (simple-expr? expr)
  (or (whnf? expr)
      #|primop applied to WHNFs|#))

