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
         (let* ((size ((self 'size)))
                (index ((self '~adjust) index size))
                (height-1 ((self '~domain) 0 size))
                (height-2 ((self '~domain) 0 (+ index 1))))
           (sync-digest
            (let loop ((node (self '(1 1))) (depth-1 1) (depth-2 1))
              (let* ((range-1 ((self '~range) depth-1 size))
                     (range-2 ((self '~range) depth-2 (+ index 1))))
                (cond ((not range-1) (sync-null))
                      ((and (equal? range-1 range-2) (= (- height-1 depth-1) (- height-2 depth-2))) node)
                      ((equal? range-1 range-2)
                       (sync-cons (sync-car node) (loop (sync-cdr node) (+ depth-1 1) (+ depth-2 1))))
                      ((> (car range-1) (car range-2)) (loop (sync-cdr node) (+ depth-1 1) depth-2))
                      ((<= (cadr range-1) (car range-2)) (loop node depth-1 (+ depth-2 1)))
                      (else (let recurse ((node (sync-car node)) (start (car range-1)) (end (cadr range-1))
                                          (rest (loop (sync-cdr node) (+ depth-1 1) (- depth-2 1))))
                              (let ((depth-start ((self '~domain) start (+ index 1)))
                                    (depth-end ((self '~domain) (- end 1) (+ index 1)))
                                    (mid (/ (+ start end) 2)))
                                (cond ((not depth-start) rest)
                                      ((equal? depth-start depth-end) (sync-cons node rest))
                                      ((>= mid (cadr range-2)) (recurse (sync-car node) start mid rest))
                                      (else (recurse (sync-cdr node) mid end
                                                     (recurse (sync-car node) start mid rest)))))))))))))

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
         (let* ((bits (lambda (x) (let loop ((i x) (b 0)) (if (<= i 0) b (loop (ash i -1) (+ b 1))))))
                (diff (+ (- size index) 1))
                (mask (- (ash 1 (- (bits diff) 1)) 1)))
           (if (>= index size) #f
               (- (bits (+ diff (logand index mask))) 1))))

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
