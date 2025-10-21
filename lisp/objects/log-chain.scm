(lambda (root)

  (define src
    '(define-class (log-chain)

       (define (*init* self)
         (let ((size-node (expression->byte-vector 0))
               (chain-node (sync-null)))
           (set! (self '(1)) (sync-cons size-node chain-node))))

       (define (size self)
         (byte-vector->expression (self '(1 0))))

       (define (get self index)
         (let* ((size ((self 'size)))
                (index ((self '~adjust) index size))
                (level ((self '~domain) index size))
                (range ((self '~range) level size))
                (modifier (if (= (- (cadr range) (car range)) (expt 2 level)) 0 1))
                (offset (modulo index (expt 2 level))))
           (let loop-1 ((node (self '(1 1))) (depth 1))
             (if (= depth level)
                 (let loop-2 ((node (sync-car node)) (depth (- depth modifier)) (offset offset))
                   (if (= depth 0) node
                       (if (< offset (expt 2 (- depth 1)))
                           (loop-2 (sync-car node) (- depth 1) (modulo offset (expt 2 (- depth 1))))
                           (loop-2 (sync-cdr node) (- depth 1) (modulo offset (expt 2 (- depth 1)))))))
                 (loop-1 (sync-cdr node) (+ depth 1))))))

       (define* (digest self (index (- ((self 'size)) 1)))
         (let ((size ((self 'size)))
               (index ((self '~adjust) index)))
           (sync-digest
            (let loop ((node (self '(1 1))) (depth-current 1) (depth-target 1))
              (let* ((range-current ((self '~range) depth-current size))
                     (range-target ((self '~range) depth-target (+ index 1))))
                (cond ((not range-current) (sync-null))
                      ((equal? range-current range-target) (if (= depth-current depth-target) node (sync-cdr node)))
                      ((< (cadr range-target) (car range-current)) (loop (sync-cdr node) (+ depth-current 1) depth-target))
                      ((> (car range-current) (car range-target)) (loop (sync-cdr node) (+ depth-current 1) (+ depth-target 1)))
                      ((= (car range-current) (car range-target))
                       (let recurse ((node (sync-car node)) (start (car range-current)) (end (cadr range-current))
                                     (target (loop (sync-cdr node) (+ depth-current 1) (+ depth-target 1))))
                         (let ((depth-start ((self '~domain) start (+ index 1)))
                               (depth-end ((self '~domain) (- end 1) (+ index 1))))
                           (cond ((not depth-start) target)
                                 ((equal? depth-start depth-end) (sync-cons node target))
                                 (else (let ((mid (/ (+ start end)  2)))
                                         (recurse (sync-cdr node) mid end
                                                  (recurse (sync-car node) start (- mid 1) target))))))))
                      (else (error 'logic-error "Unhandled logic condition"))))))))

       (define (push! self data)
         (let* ((size ((self 'size)))
                (chain (let loop ((node (self '(1 1))) (depth 1) (new data))
                         (if (sync-null? node) (sync-cons new (sync-null))
                             (let ((old (sync-car node)) (rest (sync-cdr node))
                                   (range ((self '~range) depth size)))
                               (cond ((< (- (cadr range) (car range)) (expt 2 depth)) (sync-cons (sync-cons old new) rest))
                                     ((sync-stub? old) (sync-cons (sync-cut new) (loop rest (+ depth 1) old)))
                                     (else (sync-cons new (loop rest (+ depth 1) old)))))))))
           (set! (self '(1)) (sync-cons (expression->byte-vector (+ size 1)) chain))))

       (define (set! self index data)
         (let* ((size ((self 'size)))
                (index ((self '~adjust) index size))
                (level ((self '~domain) index size))
                (range ((self '~range) level size))
                (modifier (if (= (- (cadr range) (car range)) (expt 2 level)) 0 1))
                (offset (modulo index (expt 2 level))))
           (set! (self '(1 1))
                 (let loop-1 ((node (self '(1 1))) (depth 1))
                   (if (= depth level)
                       (sync-cons (let loop-2 ((node (sync-car node)) (depth (- depth modifier)) (offset offset))
                                    (if (= depth 0) data
                                        (if (< offset (expt 2 (- depth 1)))
                                            (sync-cons (loop-2 (sync-car node) (- depth 1)
                                                               (modulo offset (expt 2 (- depth 1))))
                                                       (sync-cdr node))
                                            (sync-cons (sync-car node)
                                                       (loop-2 (sync-cdr node) (- depth 1)
                                                               (modulo offset (expt 2 (- depth 1))))))))
                                  (sync-cdr node))
                       (sync-cons (sync-car node) (loop-1 (sync-cdr node) (+ depth 1))))))))

       (define (truncate! self depth)
         (let loop ((node (self '(1 1))) (d 0)) 
           (if (sync-null? node) node
               (let ((data (sync-car node)) (rest (sync-cdr node)))
                 (if (<= d depth) (sync-cons data (loop rest (+ depth 1)))
                     (sync-cons (sync-cut data) (loop rest (- d 1))))))))

       (define* (~domain self index (size ((self 'size))))
         "Returns the depth and offset (within the level) of the specified element at the specified chain size"
         (let* ((bits (lambda (x) (let loop ((i x) (b 0)) (if (= i 0) b (loop (ash i -1) (+ b 1))))))
                (diff (+ (- size index) 1))
                (mask (- (ash 1 (- (bits diff) 1)) 1)))
           (- (bits (+ diff (logand index mask))) 1)))

       (define* (~range self depth (size ((self 'size))))
         "Returns the starting index number indices to the end index at the given depth and path"
         (if (< size (- (expt 2 depth) 1)) #f
             `(,(- size (- (expt 2 depth) 1) (modulo (+ size 1) (expt 2 depth)))
               ,(- size (- (expt 2 (- depth 1)) 1) (modulo (+ size 1) (expt 2 (- depth 1)))))))

       (define* (~adjust self index (size ((self 'size))))
         (let ((index (if (< index 0) (+ size index) index)))
           (if (and (>= index 0) (< index size)) index
               (error 'index-error "Index is out of bounds"))))))
  
  ((root 'set!) '(control library log-chain) `(content ,src)))
