#lang racket/base

; (provide body step reset current main read-resolved syntaxes)
(provide (all-defined-out))

(require syntax/modresolve)
(require "transform.rkt")

(define ns (make-base-namespace))

(define (read-resolved)
  (namespace-syntax-introduce (read-syntax) ns))

(define tested-file "examples/example.scm")

(define body
  (with-input-from-file
    tested-file
    read-resolved))

(define current body)
(define (step)
  (set! current (expand-syntax-once current))
  (print current))
(define (reset [value body])
  (set! current value))

(define (iter-read f)
  (do ([stx (read-syntax) (read-syntax)])
      ((eof-object? stx))
      (f stx)))

(define (main)
  (with-input-from-file tested-file do-loop))

(define (collect)
  (define rev-list '())
  (iter-read
   (lambda (stx) (set! rev-list (cons stx rev-list))))
  (reverse rev-list))

(define syntaxes (with-input-from-file tested-file collect))

(define (do-loop)
  (iter-read
    (lambda (stx)
      (let* ([resolved (namespace-syntax-introduce stx ns)]
             [expanded (transform-decl resolved)])
        (print expanded)
        (display "\n")))))

