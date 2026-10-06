#lang racket/base

(provide (all-defined-out))

(require (only-in racket/match match)
         (only-in racket/contract define/contract cons/c or/c any/c ->))

(require "cps-expr-contracts.rkt")

(define var/c (or/c symbol? identifier?))
(define cont/c
  (or/c
   (cons/c 'term (any/c . -> . any/c))
   (cons/c 'split (cons/c var/c syntax?))
   (cons/c 'var var/c)))

(define/contract (term-cont func) (procedure? . -> . cont/c)
  (cons 'term func))
(define/contract (split-cont var expr) (var/c syntax? . -> . cont/c)
  (cons 'split (cons var expr)))
(define/contract (var-cont var) (var/c . -> . cont/c)
  (cons 'var var))
(define/contract (gen-split-cont expr) (syntax? . -> . (values var/c cont/c))
  (let* ([var (gensym)]) (values var (split-cont var expr))))
(define/contract (gen-var-cont) (-> (values var/c cont/c))
  (let* ([var (gensym)]) (values var (var-cont var))))

(define (use-let e k)
  (match k
    [(cons 'term f) (term-cont-use-let e f)]
    [(cons 'split (cons v ke)) (split-cont-use-let e v ke)]
    [(cons 'var v) (var-cont-use-let e v)]))

(define/contract (apply-cont k e) (cont/c any/c . -> . cps-expr-shallow/c)
  (match k
    [(cons 'term f) (apply-term-cont f e)]
    [(cons 'split (cons v ke)) (apply-split-cont v ke e)]
    [(cons 'var v) #`(#%app #,v #,e)]))

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
   [else #`(#,e #,(reify-term-cont f))]))

(define (apply-split-cont v ke e)
  (cond
   [(simple-expr? e) (split-cont-use-let e v ke)]
   [else #`(#,e #,(reify-split-cont v ke))]))

(define (reify-term-cont f)
  (let ([var (gensym)])
    #`(lambda (#,var) #,(f var))))

(define (reify-split-cont v ke)
  #`(lambda (#,v) #,ke))

(define (term-cont-use-let e f)
  (let ([var (gensym)])
    #`(let ([#,var #,e]) #,(f var))))

(define (split-cont-use-let e v ke)
  #`(let ([#,v #,e]) #,ke))

(define (var-cont-use-let e kvar)
  (let ([v (gensym)])
    #`(let ([#,v #,e]) (#%app #,kvar #,v))))

; TODO find another place for these
(define (whnf? stx)
  (or (number? (syntax-e stx))
      (identifier? stx)))
(define (simple-expr? stx)
  (or (whnf? stx)
      #|primop applied to WHNFs|#))

