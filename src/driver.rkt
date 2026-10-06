#lang racket/base

; (provide body step reset current main read-resolved syntaxes)
(provide (all-defined-out))

(require syntax/modresolve)

(require "transform.rkt")
(require "oracle-test.rkt")

(define ns (make-base-namespace))

(define (read-resolved)
  (namespace-syntax-introduce (read-syntax) ns))

(define tested-file "examples/example.scm")

(define body
  (with-input-from-file
    tested-file
    read-resolved))

(define (iter-read f)
  (do ([stx (read-syntax) (read-syntax)])
      ((eof-object? stx))
      (f (namespace-syntax-introduce stx ns))))

(define (main)
  (with-input-from-file tested-file do-loop))

(define (collect)
  (define rev-list '())
  (iter-read
   (lambda (stx) (set! rev-list (cons stx rev-list))))
  (reverse rev-list))

(define decls (with-input-from-file tested-file collect))

(define (do-loop)
  (define oracle-pair (make-oracle-pair))
  (iter-read
    (lambda (decl)
      (define cps-expr (transform-decl decl))
      (println cps-expr)
      (oracle-test oracle-pair decl cps-expr)
  )))

