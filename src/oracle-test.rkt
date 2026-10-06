(module oracle-test racket
  (provide make-oracle-pair oracle-test)

  (struct oracle-pair
          (oracle tested))

  (define (make-tested-namespace)
    (define ns (make-base-namespace))
    (define (define-in-ns var val)
      (namespace-set-variable-value! var val #f ns))
    (define-in-ns '#%primop (lambda (func . args) (apply func args)))
    (define-in-ns 'print-value (lambda (v) v))
    (define-in-ns 'define-value (lambda (var val) (define-in-ns var val)))
    ns)

  (define (make-oracle-pair)
    (let* ([oracle-ns (make-base-namespace)]
           [tested-ns (make-tested-namespace)])
      (oracle-pair oracle-ns tested-ns)))

  (define (should-compare? x)
    (cond
     [(void? x) #t]
     [(number? x) #t]
     [(procedure? x) #f]
     [else (raise-argument-error "wrong argument to should-compare?:" x)]))

  (define (oracle-test oracle expr cps-expr)
    (let* ([oracle-result (eval expr (oracle-pair-oracle oracle))]
           [tested-result (eval cps-expr (oracle-pair-tested oracle))])
      (if (and (should-compare? oracle-result)
               (not (eq? oracle-result tested-result)))
        (raise-user-error "tested did not match oracle" oracle-result tested-result)
        (void))))
)
