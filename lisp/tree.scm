(define-class (tree)

  (define (~key-bits self key)
    (let loop-1 ((bytes (map (lambda (x) x) (sync-hash key))) (ret '()))
      (if (null? bytes) (reverse ret)
          (let* ((byte (car bytes))
                 (as-bits (lambda (byte) 
                            (let loop-2 ((i 0) (bits '()))
                              (if (< i -7) (reverse bits)
                                  (loop-2 (- i 1) (cons (logand (ash byte i) 1) bits)))))))
            (loop-1 (cdr bytes) (append (as-bits byte) ret))))))

  (define (~dir-new self)
    (sync-null))

  (define (~dir-get self node key)
    (let loop ((node node) (bits ((self '~key-bits) key)))
      (cond ((sync-null? node) node)
            ((sync-stub? node) node)
            ((byte-vector? (sync-car node))
             (if (equal? key (sync-car node)) (sync-cdr node) (sync-null)))
            (else (if (zero? (car bits))
                      (loop (sync-car node) (cdr bits))
                      (loop (sync-cdr node) (cdr bits)))))))

  (define (~dir-set self node key value)
    (let loop-1 ((node node) (bits ((self '~key-bits) key)) (depth 0))
      (if (or (sync-null? node) (sync-stub? node)) (sync-cons key value)
          (let ((left (sync-car node)) (right (sync-cdr node)))
            (if (not (byte-vector? left))
                (if (zero? (car bits))
                    (sync-cons (loop-1 left (cdr bits) (+ depth 1)) right)
                    (sync-cons left (loop-1 right (cdr bits) (+ depth 1))))
                (if (equal? left key) (sync-cons key value)
                    (let loop-2 ((bits-new bits) (bits-old (list-tail ((self '~key-bits) left) depth)))
                      (cond ((and (zero? (car bits-new)) (zero? (car bits-old)))
                             (sync-cons (loop-2 (cdr bits-new) (cdr bits-old)) (sync-null)))
                            ((and (not (zero? (car bits-new))) (not (zero? (car bits-old))))
                             (sync-cons (sync-null) (loop-2 (cdr bits-new) (cdr bits-old))))
                            ((and (zero? (car bits-new)) (not (zero? (car bits-old))))
                             (sync-cons (sync-cons key value) node))
                            ((and (not (zero? (car bits-new))) (zero? (car bits-old)))
                             (sync-cons node (sync-cons key value)))
                            (else (error 'logic-error "Missing conditions"))))))))))

  (define (~dir-delete self node key)
    (let loop ((node node) (bits ((self '~key-bits) key)))
      (if (or (sync-null? node) (sync-stub? node)) (sync-null)
          (let ((left (sync-car node)) (right (sync-cdr node)))
            (if (byte-vector? left)
                (if (equal? key left) (sync-null) node)
                (let ((left (if (zero? (car bits)) (loop left (cdr bits)) left))
                      (right (if (zero? (car bits)) right (loop right (cdr bits)))))
                  (cond ((and (sync-null? left) (sync-null? right)) (sync-null))
                        ((and (sync-null? left) (sync-pair? right) (byte-vector? (sync-car right))) right)
                        ((and (sync-null? right) (sync-pair? left) (byte-vector? (sync-car left))) left)
                        (else (sync-cons left right)))))))))

  (define (~dir-digest self node)
    (sync-digest node))

  (define (~dir-slice self node key)
    (let loop ((node node) (bits ((self '~key-bits) key)))
      (cond ((sync-null? node) node)
            ((sync-stub? node) node)
            ((byte-vector? (sync-car node))
             (if (equal? key (sync-car node)) node
                 (sync-cons (sync-car node) (sync-cut (sync-cdr node)))))
            (else (let ((left (sync-car node)) (right (sync-cdr node)))
                    (sync-cons (if (zero? (car bits)) (loop left (cdr bits))
                                   (if (sync-null? left) left (sync-cut (sync-cut left))))
                               (if (zero? (car bits)) (if (sync-null? right) right (sync-cut right))
                                   (loop right (cdr bits)))))))))

  (define (~dir-prune self node key keep-key?)
    (let loop ((node node) (bits ((self '~key-bits) key)))
      (if (or (sync-null? node) (sync-stub? node)) (sync-null)
          (let ((left (sync-car node)) (right (sync-cdr node)))
            (if (byte-vector? left)
                (if (not (equal? left key)) node
                    (if (not keep-key?) (sync-cut node)
                        (sync-cons left (sync-cut right))))
                (let ((left (if (zero? (car bits)) (loop left (cdr bits)) left))
                      (right (if (zero? (car bits)) right (loop right (cdr bits)))))
                  (if (and (or (sync-null? left) (sync-stub? left))
                           (or (sync-null? right) (sync-stub? right)))
                      (sync-cut node)
                      (sync-cons left right))))))))

  ;; todo: is this still necessary?
  (define (~dir-merge self node-1 node-2)
    (let recurse ((node-1 node-1) (node-2 node-2))
      (cond ((and (sync-stub? node-1) (sync-stub? node-2)) node-1)
            ((and (not (sync-stub? node-1)) (sync-stub? node-2)) node-1)
            ((and (sync-stub? node-1) (not (sync-stub? node-2))) node-2)
            ((and (sync-pair? node-1) (sync-pair? node-2))
             (sync-cons (recurse (sync-car node-1) (sync-car node-2))
                        (recurse (sync-cdr node-1) (sync-cdr node-2))))
            ((equal? node-1 node-2) node-1)
            (else (error 'invalid-structure "Cannot merge incompatible structure")))))

  (define (~dir-all self node)
    (let recurse ((node node))
      (cond ((sync-null? node) '(() #t))
            ((sync-stub? node) '(() #f))
            (else (let ((left (sync-car node)) (right (sync-cdr node)))
                    (if (byte-vector? left) `((,left) #t)
                        (let ((all-l (recurse left)) (all-r (recurse right)))
                          `(,(append (car all-l) (car all-r))
                            ,(and (cadr all-l) (cadr all-r))))))))))

  (define (~dir-valid? self node)
    (let ((new (let loop ((node node) (bits '()))
                 (if (or (sync-null? node) (sync-stub? node)) node
                     (let ((left (sync-car node)) (right (sync-cdr node)))
                       (if (byte-vector? left)
                           (let* ((bits-found ((self '~key-bits) left))
                                  (prfx-length (- (length bits-found) (length bits)))
                                  (prfx (list-tail (reverse bits-found) prfx-length)))
                             (if (equal? bits prfx) node (sync-null)))
                           (sync-cons (loop left (cons 0 bits))
                                      (loop right (cons 1 bits)))))))))
      (equal? node new)))

  ;; --- helper functions ---

  (define (~key->bytes self key)
    (cond ((sync-node? key) (error 'invalid-type "Keys cannot be sync nodes"))
          ((byte-vector? key) (append #u(0) key))
          (else (append #u(1) (expression->byte-vector key)))))

  (define (~bytes->key self bytes)
    (case (bytes 0)
      ((0) (subvector bytes 1))
      ((1) (byte-vector->expression (subvector bytes 1)))
      (else (error 'invalid-type "Key type encoding not recognized"))))

  (define (~struct-tag self)
    (sync-cons (sync-null) (sync-null)))

  (define (~struct? self node)
    (and (sync-pair? node) (equal? (sync-car node) ((self '~struct-tag)))))

  (define (~r-read self path)
    (let loop ((node (self '(1))) (path path))
      (cond ((sync-null? node) node)
            ((sync-stub? node) node)
            ((null? path) node)
            (else (loop ((self '~dir-get) node (car path)) (cdr path))))))

  (define* (~r-write! self path value)
    (set! (self '(1))
          (let loop ((node (self '(1))) (path path))
            (if (null? path) value
                (let* ((key (car path))
                       (node (if (sync-null? node) ((self '~dir-new)) node))
                       (old ((self '~dir-get) node key)))
                  ((self '~dir-set) node key (loop old (cdr path))))))))

  (define (obj->node self obj)
    (cond ((sync-node? obj) obj)
          ((procedure? obj) (sync-cons ((self '~struct-tag)) (obj)))
          ((byte-vector? obj) (append #u(0) obj)) 
          (else (append #u(1) (expression->byte-vector obj)))))

  (define (node->obj self node)
    (cond ((byte-vector? node)
           (case (node 0)
             ((0) (subvector node 1))
             ((1) (byte-vector->expression (subvector node 1)))
             (else (error 'invalid-type "Type encoding unrecognized"))))
          ((sync-null? node) node)
          ((sync-stub? node) node)
          ((sync-pair? node)
           (if (not ((self '~struct?) node)) node
               (lambda () (sync-cdr node))))
          (else (error 'invalid-type "Invalid value type"))))

  (define (get self path)
    "Retrieve the data at the specified path.

         > path (list sym|vec): path from the tree root to data
         < return (sym . (list exp)): list containing the type and value of the data
             - 'object type indicates a simple lisp-serializable value
             - 'structure type indicates a complex value represented by sync-pair?
             - 'directory type indicates an intermediate directory node
               - the second item is a list of known subpath segments
               - the third item is a bool indicating whether the directory is complete
                 (i.e., none of its underlying data has been pruned)
             - 'nothing type indicates that no data is found at the path
             - 'unknown type indicates the path has been cut"
    (let ((path (map (self '~key->bytes) path)))
      (let ((obj ((self 'node->obj) ((self '~r-read) path))))
        (if (sync-node? obj)
            (cond ((sync-null? obj) '(nothing))
                  ((sync-stub? obj) '(unknown))
                  (else
                   (let ((all ((self '~dir-all) obj)))
                     `(directory ,(map (self '~bytes->key) (car all)) ,(cadr all)))))
            (cond ((procedure? obj) (obj))
                  (else obj))))))

  (define (equal? self source path)
    "Indicate whether two paths that contain identical data

         > path (list sym|vec): path from the tree root to source data
         > target (list sym|vec): path from the tree root to target data
         < return (bool): if paths are equal then #t, otherwise #f"
    (let ((source (map (self '~key->bytes) source)) (path (map (self '~key->bytes) path)))
      (equal? ((self '~r-read) source) ((self '~r-read) path))))

  (define (equivalent? self source path)
    "Indicate whether two paths point to data that was formed
         from an identical originating data structure (before possible pruning)

         > path (list sym|vec): path from the tree root to source data
         > target (list sym|vec): path from the tree root to target data
         < return (bool): if paths are equivalent then #t, otherwise #f"
    (let ((source (map (self '~key->bytes) source)) (path (map (self '~key->bytes) path)))
      (let ((val-1 ((self '~r-read) source)) (val-2 ((self '~r-read) path)))
        (cond ((and (byte-vector? val-1) (byte-vector? val-2))
               (equal? val-1 val-2))
              ((and (sync-node? val-1) (sync-node? val-2))
               (equal? (sync-digest val-1) (sync-digest val-2)))
              (else #f)))))

  (define (set! self path value)
    "Write the value to the path. Recursively generate parent
         directories if necessary. If necessary, force all parent directories
         into a new underlayed form. If the value is #f, then delete the data
         at the path and recursively delete empty parent directories as
         necessary.

         > path (list sym|vec): path from the tree root to the data
         > value (exp|sync-pair): data to be stored at the path
         < return (bool): boolean indicating success of the operation"
    (cond ((equal? value '(unknown))
           (error 'value-error "Value conflicts with key expression '(unknown)"))
          ((and (list? value) (not (null? value)) (eq? (car value) 'directory))
           (error 'value-error "Value resembles key expression pattern '(directory ..)"))
          ((equal? value '(nothing))
           (set! (self '(1))
                 (let ((path (map (self '~key->bytes) path)))
                   (let loop ((node (self '(1))) (path path))
                     (if (null? path) ((self '~dir-new))
                         (let ((child (loop ((self '~dir-get) node (car path)) (cdr path))))
                           (if (equal? child ((self '~dir-new))) ((self '~dir-delete) node (car path))
                               ((self '~dir-set) node (car path) child))))))))
          (else (let ((content (if (sync-node? value) (lambda () value) value)))
                  ((self '~r-write!) (map (self '~key->bytes) path) ((self 'obj->node) content))))))

  (define (copy! self source path)
    "Copy data from the source path to the target path.
         Recursively generate parent directories if necessary. If
         necessary, force all parent directories into a new underlayed form. If
         the value is #f, then delete the data at the path and recursively
         delete empty parent directories as necessary.

         > source (list sym|vec): path from the tree root to the source data
         > path (list sym|vec): path from the tree root to the target data
         < return (bool): boolean indicating success of the operation"
    (let ((source (map (self '~key->bytes) source)) (path (map (self '~key->bytes) path)))
      ((self '~r-write!) path ((self '~r-read) source))))

  (define* (prune! self path keep-key?)
    "Prune specified data from a directory while maintaining the
         original hashes. If executed on directory that has not been previously
         pruned or sliced, then the directory becomes an overlayed directory.

         > path (list sym|vec): path from the tree root to the target directory 
         < return (bool): boolean indicating success of the operation"
    (let ((path (map (self '~key->bytes) path)))
      (set! (self '(1))
            (let loop ((node (self '(1))) (path path))
              (if (or (sync-null? node) (null? path)) (sync-null)
                  (let ((child (loop ((self '~dir-get) node (car path))
                                     (cdr path))))
                    (if (not (sync-null? child)) ((self '~dir-set) node (car path) child)
                        ((self '~dir-prune) node (car path) keep-key?))))))))

  (define (slice! self path)
    "Prune all data from directory EXCEPT for the specified path
         while maintaining the original hashes. If executed on directory that
         has not been previously pruned or sliced, then the directory becomes
         an overlayed directory.

         > path (list sym|vec): path from the tree root to the target directory 
         < return (bool): boolean indicating success of the operation"
    (let ((path (map (self '~key->bytes) path)))
      (set! (self '(1))
            (let loop ((node (self '(1))) (path path))
              (cond ((null? path) node)
                    ((byte-vector? node) node)
                    (((self '~struct?) node) node)
                    (else (let ((key (car path)))
                            ((self '~dir-slice) ((self '~dir-set) node key (loop ((self '~dir-get) node key) (cdr path)))
                             key))))))))

  ;; todo: is this still necessary?
  (define (merge! self other)
    "Recursively combine data from two equivalent directories.

         > source (list sym|vec): path from the tree root to the source directory 
         > path (list sym|vec): path from the tree root to the target directory 
         < return (bool): boolean indicating success of the operation"
    (let ((node-1 (self '(1))) (node-2 (other '(1))))
      (if (or (sync-null? node-1) (not (equal? (sync-digest node-1) (sync-digest node-2)))) #f
          (set! (self '(1))
                (let loop-1 ((n-1 node-1) (n-2 node-2))
                  (cond ((byte-vector? n-1) n-1)
                        ((sync-null? n-1) n-2)
                        ((sync-null? n-2) n-1)
                        ((sync-stub? n-1) n-2)
                        ((sync-stub? n-2) n-1)
                        (((self '~struct?) n-1) n-1)
                        (else (let ((n-3 ((self '~dir-merge) n-1 n-2)))
                                (let loop-2 ((n-3 n-3) (keys (car ((self '~dir-all) n-3))))
                                  (if (null? keys) n-3
                                      (let* ((k (car keys))
                                             (v-1 ((self '~dir-get) n-1 k))
                                             (v-2 ((self '~dir-get) n-2 k))
                                             (v-3 (loop-1 v-1 v-2)))
                                        (loop-2 ((self '~dir-set) n-3 k v-3)
                                                (cdr keys)))))))))))))

  (define (valid? self)
    (let loop-1 ((node (self '(1))))
      (cond ((sync-null? node) #t)
            ((sync-stub? node) #t)
            ((byte-vector? node) #t)
            (((self '~struct?) node) #t)
            ((not ((self '~dir-valid?) node)) #f)
            (else (let loop-2 ((keys (car ((self '~dir-all) node))))
                    (if (null? keys) #t
                        (if (not (loop-2 (cdr keys))) #f
                            (loop-1 ((self '~dir-get) node (car keys)))))))))))
