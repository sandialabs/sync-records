(macro (path)
  (define src
    '(define-class (linear-chain)

       (define (*init* self)
         (let ((size-node (expression->byte-vector 0))
               (chain-node (sync-null)))
           (set! (self '(1)) (sync-cons size-node chain-node))))

       (define (get self index)
         (let ((index ((self '~adjust) index)))
           (let loop ((node (self '(1 1))) (i (- ((self 'size)) 1)))
             (if (= i index) (sync-car node)
                 (loop (sync-cdr node) (- i 1))))))

       (define (previous self index)
         (let* ((index ((self '~adjust) index))
                (main (self (make-list (+ (- ((self 'size)) index) 1) 1))))
           ((eval (byte-vector->expression (self '(0))))
            (sync-cons (self '(0)) (sync-cons (expression->byte-vector (+ index 1)) main)))))

       (define* (digest self (index (- ((self 'size)) 1)))
         (sync-digest (((self 'previous) index))))

       (define (size self)
         (byte-vector->expression (self '(1 0))))

       (define (index self index~)
         ((self '~adjust) index~))

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

       (define (slice! self index)
         (let ((index ((self '~adjust) index)))
           (let ((chain (let loop ((node (self '(1 1))) (i (- ((self 'size)) 1)))
                          (if (= i index) 
                              (sync-cons (sync-car node) (sync-cut (sync-cdr node)))
                              (sync-cons (sync-cut (sync-car node)) (loop (sync-cdr node) (- i 1)))))))
             (set! (self '(1 1)) chain))))

       (define (prune! self index)
         (let ((index ((self '~adjust) index)))
           (let ((chain (let loop ((node (self '(1 1))) (i (- ((self 'size)) 1)))
                          (if (= i index) 
                              (sync-cons (sync-cut (sync-car node)) (sync-cdr node))
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

       (define (~adjust self index)
         (let* ((size ((self 'size)))
                (index (if (< index 0) (+ size index) index)))
           (if (and (>= index 0) (< index size)) index
               (error 'index-error "Index is out of bounds"))))))

  `(lambda (root)
     ((root 'set!) ,path ',src)))
