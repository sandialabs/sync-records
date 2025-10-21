(lambda (root)

  (define src
    '(define-class (history-chain)

       (define (*init* self)
         (let ((size-node (expression->byte-vector 0))
               (chain-node (sync-null)))
           (set! (self '(1)) (sync-cons size-node chain-node))))

       (define (size self)
         (byte-vector->expression (self '(1 0))))

       (define (~bits self n)
         (let* ((size ((self 'size)))
                (depth (ceiling (log (max 1 size) 2)))
                (bits (let loop ((n n) (r '())) (if (zero? n) r (loop (ash n -1) (cons (logand n 1) r))))))
           (append (make-list (- depth (length bits)) 0) bits)))

       (define* (~zip self node index (op-leaf (lambda (x) x)) (op-left sync-car) (op-right sync-cdr))
         (let loop ((node node) (bits ((self '~bits) index)))
           (cond ((null? bits) (op-leaf node))
                 ((sync-stub? node) node)
                 (else (let ((left (if (sync-null? node) (sync-null) (op-left node)))
                             (right (if (sync-null? node) (sync-null) (op-right node))))
                         (if (zero? (car bits))
                             (sync-cons (loop left (cdr bits)) right)
                             (sync-cons left (loop right (cdr bits)))))))))

       (define (push! self data)
         (let ((size ((self 'size))))
           (if (and (> size 0) (= (logand size (- size 1)) 0))
               (set! (self '(1)) (sync-cons (expression->byte-vector (+ size 1))
                                            (sync-cons (self '(1 1)) (sync-null)))))
           (set! (self '(1)) (sync-cons (expression->byte-vector (+ size 1))
                                        ((self '~zip) (self '(1 1)) size (lambda (x) data))))))

       (define (get self index)
         (let ((index ((self '~adjust) index)))
           (let loop ((node (self '(1 1))) (bits ((self '~bits) index)))
             (cond ((null? bits) node)
                   ((zero? (car bits)) (loop (sync-car node) (cdr bits)))
                   (else (loop (sync-cdr node) (cdr bits)))))))

       (define (set! self index data)
         (let ((index ((self '~adjust) index)))
           (set! (self '(1 1)) ((self '~zip) (self '(1 1)) index (lambda (x) data)))))

       (define* (digest self index)
         (let* ((size ((self 'size)))
                (index ((self '~adjust) (if index index (- size 1))))
                (num-bits (lambda (x) (let loop ((i x) (b 0)) (if (= i 0) b (loop (ash i -1) (+ b 1))))))
                (chain ((self '~zip) (self '(1 1)) index :op-right (lambda (x) (sync-null)))))
           (let loop ((node chain) (depth (- (num-bits (- size 1)) (num-bits index))))
             (if (zero? depth) (sync-digest node)
                 (loop (sync-car node) (- depth 1))))))

       (define (truncate! self index)
         (let ((index ((self '~adjust) index)))
           (set! (self '(1 1)) ((self '~zip) (self '(1 1)) index :op-left (lambda (x) (sync-cut x))))))

       (define (~adjust self index)
         (let* ((size ((self 'size)))
                (index (if (< index 0) (+ size index) index)))
           (if (and (>= index 0) (< index size)) index
               (error 'index-error "Index is out of bounds"))))))

  ((root 'set!) '(control library history-chain) `(content ,src)))
