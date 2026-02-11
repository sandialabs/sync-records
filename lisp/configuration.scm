(define-class (configuration)
  ;; Store configuration-type information in a single lisp expression in the data field
  ;; - The empty list () is both the default option for no return and the signal to delete a field

  (define* (*init* self (config '()))
    (set! (self '(1)) (expression->byte-vector config)))

  (define (get self path)
    (let loop ((config (byte-vector->expression (self '(1)))) (path path))
      (if (null? path) config
          (let ((match (assoc (car path) config)))
            (if (not match) '()
                (loop (cadr match) (cdr path)))))))

  (define (set! self path value)
    (set! (self '(1))
          (expression->byte-vector
           (let loop-1 ((config (byte-vector->expression (self '(1)))) (path path))
             (if (null? path) value
                 (let loop-2 ((config config))
                   (cond ((null? config)
                          (if (eq? value '()) '()
                              (list (list (car path) (loop-1 '() (cdr path))))))
                         ((eq? (caar config) (car path))
                          (let ((result (loop-1 (cadar config) (cdr path))))
                            (if (eq? result '()) (cdr config)
                                (cons (list (car path) result) (cdr config)))))
                         (else (cons (car config) (loop-2 (cdr config))))))))))))
