(module cps-expr-contracts racket
  (provide (all-defined-out))
  (define cps-expr-shallow/c
    (syntax/c
      (or/c
        (cons/c identifier? #|this does close to nothing|# (cons/c identifier? any/c)))))
)

