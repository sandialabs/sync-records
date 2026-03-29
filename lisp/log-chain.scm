(define-class (log-chain)
  ;; Log-chain class stores items in a log-structured tree for efficient proofs.

  (define-method (*init* self)
    ;; Initialize empty log-chain with size 0 and null chain.
    ;;   Returns:
    ;;     boolean: #t after mutation.
    (let ((size-node (expression->byte-vector 0))
          (chain-node (sync-null)))
      (set! (self '(1)) (sync-cons size-node chain-node))))

  (define-method (size self)
    ;; Return number of elements in the chain.
    ;;   Returns:
    ;;     integer: chain size.
    (byte-vector->expression (self '(1 0))))

  (define-method (index self index~)
    ;; Normalize index with bounds checking.
    ;;   Args:
    ;;     index~ (integer): index to normalize.
    ;;   Returns:
    ;;     integer: normalized index.
    ((self '~adjust) index~))

  (define-method (get self index)
    ;; Get element at index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;   Returns:
    ;;     sync node: element at index.
    (let* ((size ((self 'size)))
           (index ((self '~adjust) index size))
           (level ((self '~range) index size))
           (domain ((self '~domain) level size))
           (modifier (if (= (- (cadr domain) (car domain)) (expt 2 level)) 0 1))
           (offset (modulo index (expt 2 level))))
      (let loop-1 ((node (self '(1 1))) (depth 1))
        (if (= depth level)
            (let loop-2 ((node (sync-car node)) (depth (- depth modifier)) (offset offset))
              (if (= depth 0) node
                  (if (< offset (expt 2 (- depth 1)))
                      (loop-2 (sync-car node) (- depth 1) (modulo offset (expt 2 (- depth 1))))
                      (loop-2 (sync-cdr node) (- depth 1) (modulo offset (expt 2 (- depth 1)))))))
            (loop-1 (sync-cdr node) (+ depth 1))))))

  (define-method (previous self index)
    ;; Build a proof chain ending at index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;   Returns:
    ;;     sync node: proof chain node with header.
    (let* ((size ((self 'size)))
           (index ((self '~adjust) index size))
           (height-1 ((self '~range) 0 size))
           (height-2 ((self '~range) 0 (+ index 1)))
           (main (let loop ((node (self '(1 1))) (depth-1 1) (depth-2 1))
                   (let* ((domain-1 ((self '~domain) depth-1 size))
                          (domain-2 ((self '~domain) depth-2 (+ index 1))))
                     (cond ((not domain-1) (sync-null))
                           ((and (equal? domain-1 domain-2) (= (- height-1 depth-1) (- height-2 depth-2))) node)
                           ((equal? domain-1 domain-2)
                            (sync-cons (sync-car node) (loop (sync-cdr node) (+ depth-1 1) (+ depth-2 1))))
                           ((> (car domain-1) (car domain-2)) (loop (sync-cdr node) (+ depth-1 1) depth-2))
                           ((<= (cadr domain-1) (car domain-2)) (loop node depth-1 (+ depth-2 1)))
                           (else (let recurse ((node (sync-car node)) (start (car domain-1)) (end (cadr domain-1))
                                               (rest (loop (sync-cdr node) (+ depth-1 1) (- depth-2 1))))
                                   (let ((depth-start ((self '~range) start (+ index 1)))
                                         (depth-end ((self '~range) (- end 1) (+ index 1)))
                                         (mid (/ (+ start end) 2)))
                                     (cond ((not depth-start) rest)
                                           ((equal? depth-start depth-end) (sync-cons node rest))
                                           ((>= mid (cadr domain-2)) (recurse (sync-car node) start mid rest))
                                           (else (recurse (sync-cdr node) mid end
                                                          (recurse (sync-car node) start mid rest))))))))))))
      (sync-cons (self '(0)) (sync-cons (expression->byte-vector (+ index 1)) main))))

  (define-method (digest self (index (- ((self 'size)) 1)))
    ;; Digest of proof chain at index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;   Returns:
    ;;     byte-vector: digest.
    (let ((index ((self '~adjust) index)))
      (sync-digest ((self '~previous) index))))

  (define-method (push! self data)
    ;; Append data to the chain.
    ;;   Args:
    ;;     data (sync node): element to append.
    ;;   Returns:
    ;;     boolean: #t after mutation.
    (let* ((size ((self 'size)))
           (chain (let loop ((node (self '(1 1))) (depth 1) (new data))
                    (if (sync-null? node) (sync-cons new (sync-null))
                        (let ((old (sync-car node)) (rest (sync-cdr node))
                              (domain ((self '~domain) depth size)))
                          (cond ((< (- (cadr domain) (car domain)) (expt 2 depth)) (sync-cons (sync-cons old new) rest))
                                ((sync-stub? old) (sync-cons (sync-cut new) (loop rest (+ depth 1) old)))
                                (else (sync-cons new (loop rest (+ depth 1) old)))))))))
      (set! (self '(1)) (sync-cons (expression->byte-vector (+ size 1)) chain))))

  (define-method (set! self index data)
    ;; Replace element at index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;     data (sync node): replacement element.
    ;;   Returns:
    ;;     boolean: #t after mutation.
    (let* ((size ((self 'size)))
           (index ((self '~adjust) index size))
           (level ((self '~range) index size))
           (domain ((self '~domain) level size))
           (modifier (if (= (- (cadr domain) (car domain)) (expt 2 level)) 0 1))
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

  (define-method (slice! self index)
    ;; Slice chain to reveal proof for index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;   Returns:
    ;;     boolean: #t after mutation.
    (let* ((size ((self 'size)))
           (index ((self '~adjust) index size))
           (level ((self '~range) index size))
           (domain ((self '~domain) level size))
           (modifier (if (= (- (cadr domain) (car domain)) (expt 2 level)) 0 1))
           (offset (modulo index (expt 2 level))))
      (set! (self '(1 1))
            (let loop-1 ((node (self '(1 1))) (depth 1))
              (if (= depth level)
                  (sync-cons (let loop-2 ((node (sync-car node)) (depth (- depth modifier)) (offset offset))
                               (if (= depth 0) node
                                   (if (< offset (expt 2 (- depth 1)))
                                       (sync-cons (loop-2 (sync-car node) (- depth 1)
                                                          (modulo offset (expt 2 (- depth 1))))
                                                  (sync-cut (sync-cdr node)))
                                       (sync-cons (sync-cut (sync-car node))
                                                  (loop-2 (sync-cdr node) (- depth 1)
                                                          (modulo offset (expt 2 (- depth 1))))))))
                             (sync-cut (sync-cdr node)))
                  (sync-cons (sync-cut (sync-car node)) (loop-1 (sync-cdr node) (+ depth 1))))))))

  (define-method (prune! self index)
    ;; Prune chain to hide proof for index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;   Returns:
    ;;     boolean: #t after mutation.
    (let* ((size ((self 'size)))
           (index ((self '~adjust) index size))
           (level ((self '~range) index size))
           (domain ((self '~domain) level size))
           (modifier (if (= (- (cadr domain) (car domain)) (expt 2 level)) 0 1))
           (offset (modulo index (expt 2 level)))
           (sync-cons/cut (lambda (x y) (if (and (sync-stub? x) (sync-stub? y)) (sync-cut (sync-cons x y)) (sync-cons x y)))))
      (set! (self '(1 1))
            (let loop-1 ((node (self '(1 1))) (depth 1))
              (if (= depth level)
                  (sync-cons (let loop-2 ((node (sync-car node)) (depth (- depth modifier)) (offset offset))
                               (cond ((sync-stub? node) node)
                                     ((= depth 0) (sync-cut node))
                                     (else (if (< offset (expt 2 (- depth 1)))
                                               (sync-cons/cut (loop-2 (sync-car node) (- depth 1)
                                                                      (modulo offset (expt 2 (- depth 1))))
                                                              (sync-cdr node))
                                               (sync-cons/cut (sync-car node)
                                                              (loop-2 (sync-cdr node) (- depth 1)
                                                                      (modulo offset (expt 2 (- depth 1)))))))))
                             (sync-cdr node))
                  (sync-cons (sync-car node) (loop-1 (sync-cdr node) (+ depth 1))))))))

  (define-method (truncate! self depth)
    ;; Truncate proof tree depth by cutting deeper nodes.
    ;;   Args:
    ;;     depth (integer): max depth to keep.
    ;;   Returns:
    ;;     sync node: truncated proof tree.
    (let loop ((node (self '(1 1))) (d 0)) 
      (if (sync-null? node) node
          (let ((data (sync-car node)) (rest (sync-cdr node)))
            (if (<= d depth) (sync-cons data (loop rest (+ d 1)))
                (sync-cons (sync-cut data) (loop rest (- d 1))))))))

  (define-method (~previous self index)
    ;; Helper method to calculate previous state.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;   Returns:
    ;;     sync-node: previous state.
    (let* ((size ((self 'size)))
           (height-1 ((self '~range) 0 size))
           (height-2 ((self '~range) 0 (+ index 1))))
      (let loop ((node (self '(1 1))) (depth-1 1) (depth-2 1))
        (let* ((domain-1 ((self '~domain) depth-1 size))
               (domain-2 ((self '~domain) depth-2 (+ index 1))))
          (cond ((not domain-1) (sync-null))
                ((and (equal? domain-1 domain-2) (= (- height-1 depth-1) (- height-2 depth-2))) node)
                ((equal? domain-1 domain-2)
                 (sync-cons (sync-car node) (loop (sync-cdr node) (+ depth-1 1) (+ depth-2 1))))
                ((> (car domain-1) (car domain-2)) (loop (sync-cdr node) (+ depth-1 1) depth-2))
                ((<= (cadr domain-1) (car domain-2)) (loop node depth-1 (+ depth-2 1)))
                (else (let recurse ((node (sync-car node)) (start (car domain-1)) (end (cadr domain-1))
                                    (rest (loop (sync-cdr node) (+ depth-1 1) (- depth-2 1))))
                        (let ((depth-start ((self '~range) start (+ index 1)))
                              (depth-end ((self '~range) (- end 1) (+ index 1)))
                              (mid (/ (+ start end) 2)))
                          (cond ((not depth-start) rest)
                                ((equal? depth-start depth-end) (sync-cons node rest))
                                ((>= mid (cadr domain-2)) (recurse (sync-car node) start mid rest))
                                (else (recurse (sync-cdr node) mid end
                                               (recurse (sync-car node) start mid rest))))))))))))

  (define-method (~range self index (size ((self 'size))))
    ;; Compute level range for index in log tree.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;     size (integer): chain size.
    ;;   Returns:
    ;;     integer: level or #f.
    (let* ((bits (lambda (x) (let loop ((i x) (b 0)) (if (<= i 0) b (loop (ash i -1) (+ b 1))))))
           (diff (+ (- size index) 1))
           (mask (- (ash 1 (- (bits diff) 1)) 1)))
      (if (>= index size) #f
          (- (bits (+ diff (logand index mask))) 1))))

  (define-method (~domain self depth (size ((self 'size))))
    ;; Compute index domain bounds for a given depth.
    ;;   Args:
    ;;     depth (integer): tree depth.
    ;;     size (integer): chain size.
    ;;   Returns:
    ;;     list: (start end) or #f.
    (if (< size (- (expt 2 depth) 1)) #f
        `(,(- size (- (expt 2 depth) 1) (modulo (+ size 1) (expt 2 depth)))
          ,(- size (- (expt 2 (- depth 1)) 1) (modulo (+ size 1) (expt 2 (- depth 1)))))))

  (define-method (~adjust self index (size ((self 'size))))
    ;; Normalize index into [0,size) or raise.
    ;;   Args:
    ;;     index (integer): index to normalize.
    ;;     size (integer): chain size.
    ;;   Returns:
    ;;     integer: normalized index.
    (let ((index (if (< index 0) (+ size index) index)))
      (if (and (>= index 0) (< index size)) index
          (error 'index-error "Index is out of bounds")))))
