#lang racket/base

; TODO explicit export list
(provide (all-defined-out))

(require "transform.rkt")
(require "oracle-test.rkt")

(define ns (make-base-namespace))

(define tested-file "examples/example.scm")

(define body
  (with-input-from-file
    tested-file
    read))

(define (iter-read f)
  (do ([decl (read) (read)])
      ((eof-object? decl))
      (f decl)))

(define (main)
  (with-input-from-file tested-file do-loop))

(define (collect)
  (define rev-list '())
  (iter-read
   (lambda (decl) (set! rev-list (cons decl rev-list))))
  (reverse rev-list))

(define decls (with-input-from-file tested-file collect))

(define (do-loop)
  (define oracle-pair (make-oracle-pair))
  (iter-read
    (lambda (decl)
      (println decl)
      (define cps-expr (transform-decl decl))
      (println cps-expr)
      (oracle-test oracle-pair decl cps-expr)
  )))

