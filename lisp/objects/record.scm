(define-class (record)

  (define-class (directory blah blah)  ;; need to make any top-level imports visible here

    (define (new) #t)

    (define (get) #t)

    (define (set) #t)

    (define (all) #t)

    (define (slice) #t)

    (define (prune) #t)

    (define (merge) #t))

  (define (get) #t)

  (define (write! path value)
    (set! (record)
          (let loop ((node (root-get)) (path path))
            (if (null? path) value
                (let* ((key (car path))
                       (node (if (sync-null? node) ((directory 'new) node))
                       (old ((directory 'get) node key)))
                  ((directory 'set) node key (loop old (cdr path))))))))

  (define (set! path value)
    (if (eq? value #f)
        (root-set!
         (let ((path (map key->bytes path)))
           (let loop ((node (root-get)) (path path))
             (if (null? path) (dir-new)
                 (let ((child (loop ((dir 'get) node (car path)) (cdr path))))
                   (if (equal? child (dir-new)) (dir-delete node (car path))
                       ((dir 'set) node (car path) child)))))))
        (let ((value (if (sync-node? value) (lambda () value) value)))
          ((record 'write!) (map key->bytes path) (obj->node value)))))

  (define (copy!) #t)

  (define (equal?) #t)

  (define (equivalent?) #t)

  (define (prune!) #t)

  (define (slice!) #t)

  (define (merge!) #t)))
