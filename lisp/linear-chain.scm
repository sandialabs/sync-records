(define-class (linear-chain)
  ;; Linear-chain class stores items in a simple linked list.

  (define-method (*init* self)
    ;; Initialize empty chain with size 0 and null list.
    ;;   Returns:
    ;;     boolean: #t after mutation.
    (let ((size-node (expression->byte-vector 0))
          (chain-node (sync-null)))
      (set! (self '(1)) (sync-cons size-node chain-node))))

  (define-method (get self index)
    ;; Get element at index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;   Returns:
    ;;     sync node: element at index.
    (let ((index ((self '~adjust) index)))
      (let loop ((node (self '(1 1))) (i (- ((self 'size)) 1)))
        (if (= i index) (sync-car node)
            (loop (sync-cdr node) (- i 1))))))

  (define-method (previous self index)
    ;; Build a proof chain ending at index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;   Returns:
    ;;     chain object: proof chain with header.
    (let* ((index ((self '~adjust) index))
           (main (self (make-list (+ (- ((self 'size)) index) 1) 1))))
      ((eval (byte-vector->expression (self '(0))))
       (sync-cons (self '(0)) (sync-cons (expression->byte-vector (+ index 1)) main)))))

  (define-method (digest self (index (- ((self 'size)) 1)))
    ;; Digest of proof chain at index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;   Returns:
    ;;     byte-vector: digest.
    (sync-digest (((self 'previous) index))))

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

  (define-method (push! self data)
    ;; Append data to the chain.
    ;;   Args:
    ;;     data (sync node): element to append.
    ;;   Returns:
    ;;     boolean: #t after mutation.
    (let ((size ((self 'size))))
      (set! (self '(1)) (sync-cons (expression->byte-vector (+ size 1))
                                   (sync-cons data (self '(1 1)))))))

  (define-method (set! self index data)
    ;; Replace element at index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;     data (sync node): replacement element.
    ;;   Returns:
    ;;     boolean: #t after mutation.
    (let ((index ((self '~adjust) index)))
      (let ((chain (let loop ((node (self '(1 1))) (i (- ((self 'size)) 1)))
                     (if (= i index) 
                         (sync-cons data (sync-cdr node))
                         (sync-cons (sync-car node) (loop (sync-cdr node) (- i 1)))))))
        (set! (self '(1 1)) chain))))

  (define-method (slice! self index)
    ;; Slice chain to reveal proof for index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;   Returns:
    ;;     boolean: #t after mutation.
    (let ((index ((self '~adjust) index)))
      (let ((chain (let loop ((node (self '(1 1))) (i (- ((self 'size)) 1)))
                     (if (= i index) 
                         (sync-cons (sync-car node) (sync-cut (sync-cdr node)))
                         (sync-cons (sync-cut (sync-car node)) (loop (sync-cdr node) (- i 1)))))))
        (set! (self '(1 1)) chain))))

  (define-method (prune! self index)
    ;; Prune chain to hide proof for index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;   Returns:
    ;;     boolean: #t after mutation.
    (let ((index ((self '~adjust) index)))
      (let ((chain (let loop ((node (self '(1 1))) (i (- ((self 'size)) 1)))
                     (if (= i index) 
                         (sync-cons (sync-cut (sync-car node)) (sync-cdr node))
                         (sync-cons (sync-car node) (loop (sync-cdr node) (- i 1)))))))
        (set! (self '(1 1)) chain))))

  (define-method (truncate! self index)
    ;; Truncate chain after index and return cut tail.
    ;;   Args:
    ;;     index (integer): index to keep as last.
    ;;   Returns:
    ;;     sync node: cut tail starting after index.
    (let ((index ((self '~adjust) index))
          (garbage (sync-null)))
      (let ((chain (let loop ((node (self '(1 1))) (i (- ((self 'size)) 1)))
                     (if (= i index)
                         (begin (set! garbage node) (sync-cut node))
                         (sync-cons (sync-car node) (loop (sync-cdr node) (- i 1)))))))
        (set! (self '(1 1)) chain) garbage)))

  (define-method (~adjust self index)
    ;; Normalize index into [0,size) or raise.
    ;;   Args:
    ;;     index (integer): index to normalize.
    ;;   Returns:
    ;;     integer: normalized index.
    (let* ((size ((self 'size)))
           (index (if (< index 0) (+ size index) index)))
      (if (and (>= index 0) (< index size)) index
          (error 'index-error "Index is out of bounds")))))
