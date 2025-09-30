(lambda (root)

  (define src
    '(define-class (linear-chain)

       (define (*init* self)
         (let ((size-node (expression->byte-vector 0))
               (chain-node (sync-cons (sync-null) (sync-null))))
           (set! (self '(1)) (sync-cons size-node chain-node)))))

       (define (get self index)
         (let ((index ((self '~adjust) index)))
           (let loop ((node (self '(1 1))) (i (- ((self 'size)) 1)))
             (if (= i index) (sync-car node)
                 (loop (sync-cdr node) (- i 1))))))

       (define (size self)
         (byte-vector->expression (self '(1 0))))

       (define (push! self data)
         (let ((size ((self 'size))))
           (set! (self '(1)) (sync-cons (expression->byte-vector (+ size 1))
                                        (sync-cons data (self '(1 1)))))))

       (define (set! self index data)
         (let ((index ((self '~adjust) index)))
           (let ((chain (let loop ((node (self '(1 1))) (i (- ((self 'size)) 1)))
                          (if (= i index) 
                              (sync-cons data (sync-cdr node))
                              (sync-cons (sync-car node) (loop (sync-cdr node) (- i 1)))))))
             (set! (self '(1 1)) chain))))

       (define (truncate! self index)
         (let ((index ((self '~adjust) index))
               (garbage (sync-null)))
           (let ((chain (let loop ((node (self '(1 1))) (i (- ((self 'size)) 1)))
                          (if (= i index)
                              (begin (set! garbage node) (sync-cut node))
                              (sync-cons (sync-car node) (loop (sync-cdr node) (- i 1)))))))
             (set! (self '(1 1)) chain) garbage)))

       (define (consistent? self other)
         (let ((index-self (- ((self 'size)) 1))
               (index-other (- ((other 'size)) 1)))
           (let ((root-self (self (make-list (+ (max (- index-self index-other) 0) 2) 1)))
                 (root-other (other (make-list (+ (max (- index-other index-self) 0) 2) 1))))
             (and (equal? (sync-digest (sync-cut (self '(0))))
                          (sync-digest (sync-cut (other '(0)))))
                  (equal? (sync-digest root-self)
                          (sync-digest root-other))))))

       (define (~adjust self index)
         (let* ((size ((self 'size)))
                (index (if (< index 0) (+ size index) index)))
           (if (and (>= index 0) (< index size)) index
               (error 'index-error "Index is out of bounds"))))))

  ((root 'set!) '(control library linear-chain) `(expression ,src))

  "Installed linear chain class")
