(lambda (root)

  (define src
    '(define-class (record)

       (define (*init* self digest query)
         (set! (self '(1)) (sync-cons digest (expression->byte-vector messenger))))

       (define (get self path . rest)
         (result (eval (byte-vector->expression (self '(1 1))))))

       (define (set! self path value . rest)
         "send some kind of request I don't understand yet"
         '())))

  ((root 'set!) '(control library connection) `(content ,src)))
