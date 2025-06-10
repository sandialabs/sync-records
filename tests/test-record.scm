((test
  (begin
    (define (assert expr expt)
      (let ((str (lambda (x) (byte-vector->string (expression->byte-vector x)))))
        (let ((result (catch #t (lambda () (eval expr))
                             (lambda args (car args)))))
          (if (equal? result expt) #t
              (error 'assertion (append "Expression " (str expr)
                                        " should be " (str expt)
                                        " but was " (str result)))))))

    ;; setting
    (assert '((record 'set!) '(a b) 2) #t)
    (assert '((record 'set!) '(a c d) 4) #t)
    (assert '((record 'set!) '(a c* d) 4) #t)
    (assert '((record 'get) '(a c d)) '(object 4))
    (assert '((record 'set!) '(a e f) 9) #t)
    (assert '((record 'set!) '(a e g) 10) #t)

    ;; delete
    (assert '((record 'set!) '(a e f) #f) #t)
    (assert '((record 'set!) '(a e) #f) #t)

    ;; getting
    (assert '((record 'get) '(a)) '(directory (c c* b) #t))
    (assert '((record 'get) '(a b)) '(object 2))
    (assert '((record 'get) '(a c d)) '(object 4))
    (assert '((record 'get) '(a c* d)) '(object 4))

    ;; equality
    (assert '((record 'equal?) '(a b) '(a c)) #f)
    (assert '((record 'equal?) '(a c) '(a c*)) #t)

    ;; copying
    (assert '((record 'copy!) '(a) '(a*)) #t)
    (assert '((record 'get) '(a* c d)) '(object 4))

    ;; slicing
    (assert '((record 'get) '(a)) '(directory (c c* b) #t))
    (assert '((record 'slice!) '(a) '(b)) #t)
    (assert '((record 'get) '(a b)) '(object 2))
    (assert '((record 'get) '(a)) '(directory (b) #f))

    ;; serialization
    (assert '((record 'deserialize!) '(a*) ((record 'serialize) '(a))) #t)
    (assert '((record 'equal?) '(a* b) '(a b)) #t)
    (assert '((record 'deserialize!) '(a*) ((record 'serialize) '(a))) #t)
    (assert '((record 'equal?) '(a*) '(a)) #t)

    ;; pruning
    (assert '((record 'set!) '(b a c d) 4) #t)
    (assert '((record 'set!) '(b d d) 2) #t)
    (assert '((record 'set!) '(b d e) 5) #t)
    (assert '((record 'set!) '(b n c d) 1) #t)
    (assert '((record 'copy!) '(b) '(b*)) #t)
    (assert '((record 'prune!) '(b) '(d d) #t) #t)
    (assert '((record 'prune!) '(b) '(d e)) #t)
    (assert '((record 'get) '(b d)) '(directory (d) #f))
    (assert '((record 'get) '(b d d)) '(unknown ()))
    (assert '((record 'get) '(b n c d)) '(object 1))

    ;; equivalency
    (assert '((record 'equal?) '(b) '(b*)) #f)
    (assert '((record 'equivalent?) '(b) '(b*)) #t)

    ;; merging
    (assert '((record 'merge!) '(b) '(b*)) #t)
    (assert '((record 'equivalent?) '(b) '(b*)) #t)
    (assert '((record 'get) '(b* d d)) '(object 2))

    'success)))
