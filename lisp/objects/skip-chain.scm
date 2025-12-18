(lambda (root)

  (define src
    '(define-class (skip-chain)

       (define (*init* self)
         (let ((size-node (expression->byte-vector 0))
               (chain-node (sync-null)))
           (set! (self '(1)) (sync-cons size-node chain-node))))

       (define (size self)
         (byte-vector->expression (self '(1 0))))

       (define (~previous self head index)
         (if (< (- index 1) 0) (sync-null)
             (let loop ((node (sync-cdr head)) (depth 0))
               (if (sync-null? node) (sync-cons head (sync-null))
                   (if (not (zero? (modulo (- index 1) (expt 2 depth)))) node
                       (sync-cons head (loop (sync-cdr node) (+ depth 1))))))))

       (define (~walk self head index)
         (let loop-1 ((node (self '(1 1))) (i (- ((self 'size)) 1)))
           (cond ((= i index) node)
                 ((< i index) (error 'logic-error "Should not be here"))
                 (else (let loop-2 ((node (sync-cdr node)) (depth 0))
                         (if (or (not (zero? (modulo i (expt 2 (+ depth 1)))))
                                 (< (- i (expt 2 (+ depth 1))) index)
                                 (= (- i (expt 2 (+ depth 1))) 0))
                             (loop-1 (sync-car node) (- i (expt 2 depth)))
                             (loop-2 (sync-cdr node) (+ depth 1))))))))

       (define (push! self data)
         (let* ((size ((self 'size)))
                (prev ((self '~previous) (self '(1 1)) size)))
           (set! (self '(1)) (sync-cons (expression->byte-vector (+ size 1))
                                        (sync-cons data prev)))))

       (define (get self index)
         (sync-car ((self '~walk) (self '(1 1)) ((self '~adjust) index))))

       (define* (digest self (index (- ((self 'size)) 1)))
         (sync-digest ((self '~walk) (self '(1 1)) ((self '~adjust) index))))

       (define (set! self index data)
         (let ((size ((self 'size))) (index ((self '~adjust) index)))
           (set! (self '(1 1))
                 (let loop ((node (self '(1 1))) (i (- size 1)))
                   (if (= i index) (sync-cons data (sync-cdr node))
                       (sync-cons (sync-car node) ((self '~previous) (loop (sync-car (sync-cdr node)) (- i 1)) i)))))))

       (define (prune! self index)
         (let ((size ((self 'size))) (index ((self '~adjust) index)))
           (set! (self '(1 1))
                 (let loop ((node (self '(1 1))) (i (- size 1)))
                   (if (= i index) (sync-cons (sync-cut (sync-car node)) (sync-cdr node))
                       (sync-cons (sync-car node) ((self '~previous) (loop (sync-car (sync-cdr node)) (- i 1)) i)))))))

       (define (truncate! self index)
         (let ((size ((self 'size))) (index ((self '~adjust) index)))
           (set! (self '(1 1))
                 (let loop ((node (self '(1 1))) (i (- size 1)))
                   (if (= i index)
                       (sync-cons (sync-car node)
                                  (let cut ((n (sync-cdr node)))
                                    (if (sync-null? n) n (sync-cons (sync-cut (sync-car n)) (cut (sync-cdr n))))))
                       (sync-cons (sync-car node) ((self '~previous) (loop (sync-car (sync-cdr node)) (- i 1)) i)))))))

       (define (~adjust self index)
         (let* ((size ((self 'size)))
                (index (if (< index 0) (+ size index) index)))
           (if (and (>= index 0) (< index size)) index
               (error 'index-error "Index is out of bounds"))))))

  ((root 'set!) '(control library skip-chain) `(content ,src)))
