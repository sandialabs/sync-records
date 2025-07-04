(lambda (secret control . scripts)
  "Install the record interface into the Journal SDK. The Record
  interface is the recommended core data model for all synchronic web
  journals. It abstracts away the underlying hash binary tree by provide
  a path-based mechanism to traceably read, write, compare, and prune
  lisp-style data.

  > secret (str): the root secret used to generate cryptographic materials
  > control (fnc): control function that determining logic for end-user queries
    - functions are of the form (lambda (record secret-hash query) ...)
  > scripts (list fnc): list of functions to setup additional special logic
    - functions are of the form (lambda (record secret) ...)
  < return (str): success message"
  (define secret-hash (sync-hash (expression->byte-vector secret)))

  (define record-new
    `(lambda (root-get root-set!)
       ;; --- verifiable map structure ---

       (define (print . exprs)
         (let loop ((exprs exprs))
           (if (null? exprs) (newline)
               (begin (display (car exprs)) (display " ") (loop (cdr exprs))))))

       (define sync-null-expr (byte-vector->expression (expression->byte-vector (sync-null))))

       (define sync-struct (sync-cons (sync-null) (sync-null)))
       
       (define (key-bits key)
         (let loop-1 ((bytes (map (lambda (x) x) (sync-hash key))) (ret '()))
           (if (null? bytes) (reverse ret)
               (let* ((byte (car bytes))
                      (as-bits (lambda (byte) 
                                 (let loop-2 ((i 0) (bits '()))
                                   (if (< i -7) (reverse bits)
                                       (loop-2 (- i 1) (cons (logand (ash byte i) 1) bits)))))))
                 (loop-1 (cdr bytes) (append (as-bits byte) ret))))))

       (define (key->bytes key)
         (cond ((sync-pair? key) (error 'invalid-type "Keys cannot be sync pairs"))
               ((byte-vector? key) (append #u(0) key))
               (else (append #u(1) (expression->byte-vector key)))))

       (define (bytes->key bytes)
         (case (bytes 0)
           ((0) (subvector bytes 1))
           ((1) (byte-vector->expression (subvector bytes 1)))
           (else (error 'invalid-type "Key type encoding not recognized"))))

       (define (obj->node value)
         (cond ((sync-pair? value) value)
               ((procedure? value) (sync-cons sync-struct (value)))
               ((byte-vector? value) (append #u(0) value)) 
               (else (append #u(1) (expression->byte-vector value)))))

       (define (node->obj node)
         (cond ((boolean? node) node)
               ((byte-vector? node)
                (case (node 0)
                  ((0) (subvector node 1))
                  ((1) (byte-vector->expression (subvector node 1)))
                  (else (error 'invalid-type "Type encoding unrecognized"))))
               ((sync-pair? node)
                (if (sync-null? node) node
                    (if (not (equal? (sync-car node) sync-struct)) node
                        (lambda () (sync-cdr node)))))
               (else (error 'invalid-type "Invalid value type"))))

       (define (node->info node)
         (let ((u-digest (sync-pair->byte-vector node)))
           (if (sync-null? node) `(stub #f #f ,(sync-pair->byte-vector (sync-null)))
               (let ((left (sync-car node)) (right (sync-cdr node)))
                 (cond ((byte-vector? left) 
                        `(u-leaf ,left ,right ,u-digest))
                       ((and (sync-pair? left) (sync-pair? right))
                        `(u-branch ,left ,right ,u-digest))
                       (else (let ((type (right 0)) (digest (subvector right 1)))
                               (case type
                                 ((0) `(o-leaf ,(sync-car left) ,(sync-cdr left) ,digest))
                                 ((1) `(o-branch ,(sync-car left) ,(sync-cdr left) ,digest))
                                 ((2) `(p-leaf ,(sync-car left)
                                               ,(case ((sync-cdr left) 0)
                                                  ((0) #f)
                                                  ((1) #t)
                                                  (else (error 'invalid-type "Pruned leaf show byte unrecognized")))
                                               ,digest))
                                 ((3) `(p-branch #f #f ,digest))
                                 (else (error 'invalid-type "Byte type inference failed"))))))))))

       (define* (info->node type left right digest)
         (case type
           ((stub) (sync-null))
           ((u-leaf u-branch) (sync-cons left right))
           ((o-leaf) (sync-cons (sync-cons left right) (append #u(0) digest)))
           ((o-branch) (sync-cons (sync-cons left right) (append #u(1) digest))) 
           ((p-leaf) (sync-cons (sync-cons left (if right #u(1) #u(0)))
                                (append #u(2) digest)))
           ((p-branch) (sync-cons (sync-null) (append #u(3) digest)))
           (else (error 'invalid-type "Invalid input type"))))

       (define (leaf-hash key value)
         (sync-hash (append (sync-hash (sync-hash key))
                            (if (sync-pair? value) (cadddr (node->info value))
                                (sync-hash (sync-hash value))))))

       (define (dir-new)
         (sync-null))

       (define (dir-get node key)
         (let loop ((node node) (bits (key-bits key)))
           (let ((info (node->info node)))
             (case (car info)
               ((stub) #f)
               ((p-leaf) (equal? (cadr info) key))
               ((p-branch) #t)
               ((u-leaf o-leaf)
                (if (equal? (cadr info) key) (caddr info) #f))
               ((u-branch o-branch)
                (loop (if (zero? (car bits)) (cadr info) (caddr info)) (cdr bits)))
               (else (error 'logic-error "Missing conditions"))))))

       (define (dir-overlayed? node key value)
         (let loop ((node node) (bits (key-bits key)))
           (let ((info (node->info node)))
             (case (car info)
               ((stub) #f)
               ((p-leaf)
                (equal? (cadr info) key))
               ((p-branch)
                (error 'record-error "Cannot check overlayed for pruned branch"))
               ((u-leaf o-leaf)
                (if (equal? (cadr info) key) (equal? (cadddr info) (leaf-hash key value))
                    (error 'record-error "Cannot check overlayed for unset key")))
               ((u-branch o-branch)
                (loop (if (zero? (car bits)) (cadr info) (caddr info)) (cdr bits)))
               (else (error 'logic-error "Missing conditions"))))))

       (define (dir-set node key value)
         (let loop-1 ((node node) (bits (key-bits key)) (depth 0))
           (let ((info (node->info node)))
             (case (car info)
               ((o-leaf o-branch p-leaf p-branch) 
                (error 'invalid-operation "Cannot set incomplete directory"))
               ((stub) (info->node 'u-leaf key value))
               ((u-branch)
                (if (zero? (car bits))
                    (info->node 'u-branch (loop-1 (cadr info) (cdr bits) (+ depth 1)) (caddr info))
                    (info->node 'u-branch (cadr info) (loop-1 (caddr info) (cdr bits) (+ depth 1)))))
               ((u-leaf)
                (let ((key-old (cadr info)) (value-old (caddr info)))
                  (if (equal? key key-old)
                      (info->node 'u-leaf key value)
                      (let ((bits-old (key-bits key-old)))
                        (let loop-2 ((pat-new bits) (pat-old (list-tail bits-old depth)))
                          (cond ((and (zero? (car pat-new)) (zero? (car pat-old)))
                                 (info->node 'u-branch
                                             (loop-2 (cdr pat-new) (cdr pat-old))
                                             (info->node 'stub)))
                                ((and (not (zero? (car pat-new))) (not (zero? (car pat-old))))
                                 (info->node 'u-branch
                                             (info->node 'stub)
                                             (loop-2 (cdr pat-new) (cdr pat-old))))
                                ((and (zero? (car pat-new)) (not (zero? (car pat-old))))
                                 (info->node 'u-branch
                                             (info->node 'u-leaf key value)
                                             (info->node 'u-leaf key-old value-old)))
                                ((and (not (zero? (car pat-new))) (zero? (car pat-old)))
                                 (info->node 'u-branch
                                             (info->node 'u-leaf key-old value-old)
                                             (info->node 'u-leaf key value)))
                                (else (error 'invalid-logic "Missing conditions"))))))))
               (else (error 'logic-error "Missing conditions"))))))

       (define (dir-delete node key)
         (apply info->node
                (let loop ((node node) (bits (key-bits key)))
                  (let ((info (node->info node)))
                    (case (car info)
                      ((o-leaf o-branch p-leaf p-branch) 
                       (error (error 'invalid-operation "Cannot delete from incomplete directory")))
                      ((stub) info)
                      ((u-leaf) (if (equal? key (cadr info)) '(stub) info))
                      ((u-branch)
                       (let ((left (if (zero? (car bits)) (loop (cadr info) (cdr bits))
                                       (node->info (cadr info))))
                             (right (if (zero? (car bits)) (node->info (caddr info))
                                        (loop (caddr info) (cdr bits)))))
                         (cond ((and (eq? (car left) 'stub) (eq? (car right) 'stub)) '(stub))
                               ((and (eq? (car left) 'stub) (eq? (car right) 'u-leaf)) right)
                               ((and (eq? (car left) 'u-leaf) (eq? (car right) 'stub)) left)
                               (else `(u-branch ,(apply info->node left)
                                                ,(apply info->node right)))))))))))

       (define (dir-digest node)
         (cadddr (node->info node)))

       (define (dir-overlay? node)
         (and (sync-pair? node)
              (not (sync-null? node))
              (sync-pair? (sync-car node))
              (byte-vector? (sync-cdr node))))

       (define (dir-overlay node key value)
         (let loop ((node node) (bits (key-bits key)))
           (let ((info (node->info node)))
             (case (car info)
               ((stub) (error 'invalid-operation "Cannot update unset key"))
               ((p-branch) (error 'invalid-operation "Cannot update pruned branch"))
               ((u-leaf p-leaf)
                (cond ((not (equal? key (cadr info)))
                       (error 'invalid-operation "Cannot update unset key"))
                      ((equal? value (caddr info)) node)
                      ((equal? (cadddr info) (leaf-hash key value))
                       (info->node 'o-leaf key value (cadddr info)))
                      (else (error 'integrity-error "Cannot overlay non-authentic node"))))
               ((o-leaf) (if (equal? key (cadr info))
                             (info->node 'o-leaf key value (cadddr info))
                             (error 'invalid-operation "Cannot update unset key")))
               ((u-branch)
                (let ((left (if (zero? (car bits)) (loop (cadr info) (cdr bits))
                                (cadr info)))
                      (right (if (zero? (car bits)) (caddr info)
                                 (loop (caddr info) (cdr bits)))))
                  (if (and (equal? left (cadr info)) (equal? right (caddr info))) node
                      (info->node 'o-branch left right (cadddr info)))))
               ((o-branch)
                (let ((left (if (zero? (car bits)) (loop (cadr info) (cdr bits))
                                (cadr info)))
                      (right (if (zero? (car bits)) (caddr info)
                                 (loop (caddr info) (cdr bits)))))
                  (info->node 'o-branch left right (cadddr info))))
               (else 'logic-error "Missing conditions")))))

       (define (dir-underlay node)
         (apply info->node
                (let recurse ((node node))
                  (let ((info (node->info node)))
                    (case (car info)
                      ((stub p-leaf p-branch) '(stub))
                      ((u-leaf u-branch) info)
                      ((o-leaf) (cons 'u-leaf (cdr info)))
                      ((o-branch)
                       (let ((left (recurse (cadr info)))
                             (right (recurse (cadr info)))) 
                         (cond ((and (eq? (car left) 'stub) (eq? (car right) 'stub)) '(stub))
                               ((and (eq? (car left) 'stub) (eq? (car right) 'u-leaf)) right)
                               ((and (eq? (car left) 'u-leaf) (eq? (car right) 'stub)) left)
                               (else `(u-branch ,(apply info->node left)
                                                ,(apply info->node right))))))
                      (else logic-error "Missing conditions"))))))

       (define (dir-slice node key)
         (let loop ((node node) (bits (key-bits key)) (on-path? #t))
           (let ((info (node->info node)))
             (if (not on-path?) 
                 (case (car info)
                   ((o-leaf u-leaf) (info->node 'p-leaf (cadr info) #f (cadddr info)))
                   ((o-branch u-branch) (info->node 'p-branch #f #f (cadddr info)))
                   (else node))
                 (case (car info)
                   ((stub p-leaf p-branch u-leaf o-leaf) node)
                   ((u-branch)
                    (let ((left (loop (cadr info) (cdr bits) (zero? (car bits))))
                          (right (loop (caddr info) (cdr bits) (not (zero? (car bits))))))
                      (if (and (equal? left (cadr info)) (equal? right (caddr info))) node
                          (info->node 'o-branch left right (cadddr info)))))
                   ((o-branch)
                    (let ((left (loop (cadr info) (cdr bits) (zero? (car bits))))
                          (right (loop (caddr info) (cdr bits) (not (zero? (car bits))))))
                      (info->node 'o-branch left right (cadddr info))))
                   (else logic-error "Missing conditions"))))))

       (define (dir-prune node key keep-key?)
         (apply info->node
                (let loop ((node node) (bits (key-bits key)))
                  (let ((info (node->info node)))
                    (case (car info)
                      ((stub p-leaf p-branch) info)
                      ((u-leaf o-leaf)
                       (if (not (equal? key (cadr info))) info
                           `(p-leaf ,(cadr info) ,keep-key? ,(cadddr info))))
                      ((u-branch o-branch)
                       (let ((left (if (zero? (car bits))
                                       (loop (cadr info) (cdr bits))
                                       (node->info (cadr info))))
                             (right (if (zero? (car bits))
                                        (node->info (caddr info))
                                        (loop (caddr info) (cdr bits))))
                             (prunable (lambda (x) (or (eq? (car x) 'stub)
                                                       (eq? (car x) 'p-branch)
                                                       (and (eq? (car x) 'p-leaf)
                                                            (not (caddr x)))))))
                         (if (and (not keep-key?) (prunable left) (prunable right))
                             `(p-branch #f #f ,(cadddr info))
                             `(o-branch ,(apply info->node left)
                                        ,(apply info->node right)
                                        ,(cadddr info)))))
                      (else logic-error "Missing conditions"))))))

       (define (dir-merge node-1 node-2)
         (let recurse ((node-1 node-1) (node-2 node-2))
           (let ((info-1 (node->info node-1)) (info-2 (node->info node-2)))
             (case (car info-1)
               ((stub u-leaf u-branch) node-1)
               ((p-leaf)
                (case (car info-2)
                  ((p-leaf) node-1)
                  ((u-leaf o-leaf) node-2)
                  (else (error invalid-operation "Cannot merge incompatible directory"))))
               ((p-branch)
                (case (car info-2)
                  ((p-branch) node-1)
                  ((u-branch o-branch) node-2)
                  (else (error invalid-operation "Cannot merge incompatible directory"))))
               ((o-leaf)
                (case (car info-2)
                  ((u-leaf) node-2)
                  ((o-leaf p-leaf) node-1)
                  (else (error invalid-operation "Cannot merge incompatible directory"))))
               ((o-branch)
                (case (car info-2)
                  ((p-branch) node-1)
                  ((u-branch o-branch)
                   (let* ((left (recurse (cadr info-1) (cadr info-2)))
                          (right (recurse (caddr info-1) (caddr info-2)))
                          (type (if (equal? (sync-pair->byte-vector (sync-cons left right)) (cadddr info-1))
                                    'u-branch 'o-branch)))
                     (info->node type left right (cadddr info-1))))
                  (else (error invalid-operation "Cannot merge incompatible directory"))))
               (else (error 'logic-error "Missing conditions"))))))

       (define (dir-all node)
         (let recurse ((node node))
           (let ((info (node->info node)))
             (case (car info)
               ((stub p-branch) '())
               ((u-leaf o-leaf) `(,(cadr info)))
               ((p-leaf) (if (caddr info) `(,(cadr info)) '()))
               ((u-branch o-branch) (append (recurse (cadr info)) (recurse (caddr info))))
               (else logic-error "Missing conditions")))))

       (define (dir-align node)
         (apply info->node
                (let recurse ((node node))
                  (let ((info (node->info node)))
                    (case (car info)
                      ((stub) info)
                      ((u-leaf) 
                       `(o-leaf ,(cadr info)
                                ,(caddr info)
                                ,(if (not (dir-overlay? (caddr info))) (cadddr info)
                                     (leaf-hash (cadr info) (caddr info)))))
                      ((u-branch) 
                       (let ((left (recurse (cadr info))) (right (recurse (caddr info))))
                         `(o-branch ,(apply info->node left)
                                    ,(apply info->node right)
                                    ,(sync-hash (append (cadddr left) (cadddr right))))))
                      (else (error invalid-operation "Cannot align overlayed directory"))))))) 

       (define (dir-valid? node)
         (let ((info (let loop ((node node) (bits '()))
                       (let ((info (node->info node)))
                         (case (car info)
                           ((stub p-leaf p-branch) info)
                           ((u-leaf o-leaf)
                            (let* ((bits-found (key-bits (cadr info)))
                                   (prefix-length (- (length bits-found) (length bits)))
                                   (prefix (list-tail (reverse bits-found) prefix-length)))
                              (if (not (equal? bits prefix))
                                  (error 'verification-failure "Key does not correspond to location")
                                  info)))
                           ((u-branch o-branch)
                            (let ((left (loop (cadr info) (cons 0 bits)))
                                  (right (loop (caddr info) (cons 1 bits))))
                              `(,(car info)
                                ,(apply info->node left)
                                ,(apply info->node right)
                                ,(sync-hash (append (cadddr left) (cadddr right))))))
                           (else (error 'logic-error "Missing conditions")))))))
           (equal? node (apply info->node info))))

       (define memoizer (make-hash-table))

       (define-macro (memoize name)
         (let ((func (gensym)))
           `(begin
              (define ,func ,name)
              (define (,name . args)
                (if (not (memoizer ',name))
                    (set! (memoizer ',name) (make-hash-table)))
                (if (not (memoizer ',name args))
                    (set! (memoizer ',name args) (apply ,func args)))
                (memoizer ',name args)))))

       (memoize dir-get)
       (memoize dir-all)
       (memoize dir-valid?)

       ;; --- helper functions ---

       (define (r-read path)
         (let loop ((node (root-get)) (path path))
           (cond ((boolean? node) node)
                 ((null? path) node)
                 ((not (sync-pair? node)) #f)
                 (else (loop (dir-get node (car path)) (cdr path))))))

       (define* (r-write! path value force-under?)
         (if (not value) #f
             (root-set!
              (let loop ((node (root-get)) (path path) (over? #f))
                (if (null? path) (obj->node value)
                    (let* ((key (car path))
                           (node (if node node (dir-new)))
                           (node (if force-under? (dir-underlay node) node))
                           (old (dir-get node key)))
                      (if (and (or over? (and (sync-pair? node) (dir-overlay? node)))
                               (not force-under?))
                          (dir-overlay node key (loop old (cdr path) #t))
                          (if (sync-pair? node)
                              (dir-set node key (loop old (cdr path) #f))
                              (dir-set (dir-new) key
                                       (loop (dir-new) (cdr path) #f))))))))))

       (define (r-valid? node)
         (let loop-1 ((node node))
           (cond ((not (sync-pair? node)) #t)
                 ((equal? (sync-car node) sync-struct) #t)
                 ((not (dir-valid? node)) #f)
                 (else (let loop-2 ((keys (dir-all node)))
                         (if (null? keys) #t
                             (let ((child (dir-get node (car keys))))
                               (if (and (dir-overlay? node)
                                        (not (dir-overlayed? node (car keys) child))) #f
                                        (if (not (loop-2 (cdr keys))) #f
                                            (loop-1 child))))))))))

       (define (r-complete? path)
         (let ((node (r-read (map key->bytes path))))
           (if (not node) #t
               (not (dir-overlay? node)))))

       (define (record-get path)
         "Retrieve the data at the specified path.

         > path (list sym|vec): path from the record root to data
         < return (sym . (list exp)): list containing the type and value of the data
             - 'object type indicates a simple lisp-serializable value
             - 'structure type indicates a complex value represented by sync-pair?
             - 'directory type indicates an intermediate directory node
               - the second item is a list of known subpath segments
               - the third item is a bool indicating whether the directory is complete
                 (i.e., none of its underlying data has been pruned)
             - 'nothing type indicates that no data is found at the path
             - 'unknown type indicates the path has been pruned"
         (let ((path (map key->bytes path)))
           (let ((obj (node->obj (r-read path))))
             (cond ((not obj) '(nothing ()))
                   ((boolean? obj) '(unknown ()))
                   ((sync-pair? obj)
                    `(directory ,(map bytes->key (dir-all obj))
                                ,(not (dir-overlay? obj))))
                   ((procedure? obj) `(structure ,(obj)))
                   (else `(object ,obj))))))

       (define (record-equal? source path)
         "Indicate whether two paths that contain identical data

         > path (list sym|vec): path from the record root to source data
         > target (list sym|vec): path from the record root to target data
         < return (bool): if paths are equal then #t, otherwise #f"
         (let ((source (map key->bytes source)) (path (map key->bytes path)))
           (equal? (r-read source) (r-read path))))

       (define (record-equivalent? source path)
         "Indicate whether two paths point to data that was formed
         from an identical originating data structure (before possible pruning)

         > path (list sym|vec): path from the record root to source data
         > target (list sym|vec): path from the record root to target data
         < return (bool): if paths are equivalent then #t, otherwise #f"
         (let ((source (map key->bytes source)) (path (map key->bytes path)))
           (let ((val-1 (r-read source)) (val-2 (r-read path)))
             (equal? (if (sync-pair? val-1) (dir-digest val-1) #f)
                     (if (sync-pair? val-2) (dir-digest val-2) #f)))))

       (define (record-serialize path)
         "Obtain a serialized representation of all data under the path

         > path (list sym|vec): path from the record root to the data
         < return (exp): lisp-serialized contents"
         (let ((path (map key->bytes path)))
           (let ((node (r-read path)))
             (if (not node) #f
                 (let ((ls '())
                       (tb (hash-table))
                       (sym (lambda (x) (string->symbol (append "n-" x)))))
                   (let recurse ((node (r-read path)))
                     (let* ((h (if (sync-pair? node) (sync-pair->byte-vector node) (sync-hash node)))
                            (id (sym (byte-vector->hex-string h))))
                       (cond ((tb id) id)
                             ((and (not (byte-vector? node)) (sync-null? node)) id)
                             ((byte-vector? node) (set! (tb id) #t)
                              (set! ls (cons `(,id ,(byte-vector->hex-string node)) ls)) id)
                             (else (set! (tb id) #t)
                                   (set! ls (cons `(,id ,(recurse (sync-car node))
                                                        ,(recurse (sync-cdr node))) ls)) id))))
                   (let* ((counter 0)
                          (seen (hash-table))
                          (null (sym (byte-vector->hex-string (sync-pair->byte-vector (sync-null)))))
                          (shorten (lambda (x)
                                     (cond ((eq? x null) (sym "0"))
                                           ((not (symbol? x)) x)
                                           ((seen x) (seen x))
                                           (else (set! (seen x)
                                                       (sym (number->string
                                                             (set! counter (+ counter 1)))))))))
                          (compact (lambda (x)
                                     (if (= (length x) 2) x
                                         (list (car x) (list (cadr x) (caddr x)))))))
                     (map (lambda (x) (compact (map shorten x))) ls)))))))

       (define (record-set! path value)
         "Write the value to the path. Recursively generate parent
         directories if necessary. If necessary, force all parent directories
         into a new underlayed form. If the value is #f, then delete the data
         at the path and recursively delete empty parent directories as
         necessary.

         > path (list sym|vec): path from the record root to the data
         > value (exp|sync-pair): data to be stored at the path
         < return (bool): boolean indicating success of the operation"
         (if (eq? value #f)
             (root-set!
              (let ((path (map key->bytes path)))
                (let loop ((node (root-get)) (path path))
                  (if (or (not node) (null? path)) #f
                      (let ((child (loop (dir-get node (car path)) (cdr path))))
                        (if child (dir-set node (car path) child)
                            (let ((node-new (dir-delete node (car path))))
                              (if (equal? node-new (dir-new)) #f node-new))))))))
             (let ((path (map key->bytes path))
                   (value (if (sync-pair? value) (lambda () value) value)))
               (if (not (r-complete? path)) #f
                   (r-write! path value #t))))
         #t)

       (define (record-copy! source path)
         "Copy data from the source path to the target path.
         Recursively generate parent directories if necessary. If
         necessary, force all parent directories into a new underlayed form. If
         the value is #f, then delete the data at the path and recursively
         delete empty parent directories as necessary.

         > source (list sym|vec): path from the record root to the source data
         > path (list sym|vec): path from the record root to the target data
         < return (bool): boolean indicating success of the operation"
         (let ((source (map key->bytes source)) (path (map key->bytes path)))
           (if (or (not (r-complete? source)) (not (r-complete? path))) #f
               (let ((value (r-read source)))
                 (begin
                   (r-write! path (node->obj value) #t)
                   #t)))))

       (define (record-deserialize! path serialization)
         "Validate and write serialized data to the specified path

         > path (list sym|vec): path from the record root to the target location
         > serialization (exp): expression containing the serialized data
         < return (bool): boolean indicating success of the operation"
         (let ((path (map key->bytes path)))
           (let* ((proc (lambda (x)
                          (let ((k (car x)) (v (cadr x)))
                            (cond ((string? v) `(define ,k ,(hex-string->byte-vector v)))
                                  ((pair? v) `(define ,k (sync-cons ,(car v) ,(cadr v))))
                                  (else '())))))
                  (expr `(begin (define n-0 (sync-null))
                                ,@(map proc (reverse serialization))))
                  (node (eval expr)))
             (if (not (r-valid? node))
                 (error 'deserialization-failure "Invalid serialization expression")
                 (begin (r-write! path node #t) #t)))))
       
       (define* (record-prune! path subpath keep-key?)
         "Prune specified data from a directory while maintaining the
         original hashes. If executed on directory that has not been previously
         pruned or sliced, then the directory becomes an overlayed directory.

         > path (list sym|vec): path from the record root to the target directory 
         > subpath (exp): subpath from the target directory to target data 
         > keep-key? (bool): if #t, then retain the path segment in the parent directory
         < return (bool): boolean indicating success of the operation"
         (let ((path (map key->bytes path)) (subpath (map key->bytes subpath)))
           (if (boolean? (r-read (append path subpath))) #f
               (begin
                 (r-write!
                  path
                  (let loop ((node (r-read path)) (subpath subpath))
                    (if (or (not node) (null? subpath)) #f
                        (let ((child (loop (dir-get node (car subpath))
                                           (cdr subpath))))
                          (if child (dir-overlay node (car subpath) child)
                              (let ((node-new (dir-prune node (car subpath) keep-key?)))
                                (if (equal? node-new (dir-new)) #f node-new)))))))
                 #t))))

       (define (record-slice! path subpath)
         "Prune all data from directory EXCEPT for the specified path
         while maintaining the original hashes. If executed on directory that
         has not been previously pruned or sliced, then the directory becomes
         an overlayed directory.

         > path (list sym|vec): path from the record root to the target directory 
         > subpath (exp): subpath from the target directory to target data 
         < return (bool): boolean indicating success of the operation"
         (let ((path (map key->bytes path)) (subpath (map key->bytes subpath)))
           (r-write! path
                     (let loop ((node (r-read path)) (subpath subpath))
                       (if (or (not node) (not (sync-pair? node)) (null? subpath)) node
                           (let ((key (car subpath)))
                             (dir-slice (dir-overlay node key
                                                     (loop (dir-get node key)
                                                           (cdr subpath)))
                                        key)))))
           #t))


       (define (record-merge! source path)
         "Recursively combine data from two equivalent directories.

         > source (list sym|vec): path from the record root to the source directory 
         > path (list sym|vec): path from the record root to the target directory 
         < return (bool): boolean indicating success of the operation"
         (let ((source (map key->bytes source)) (path (map key->bytes path)))
           (let ((node-1 (r-read source)) (node-2 (r-read path)))
             (if (and (not (boolean? node-1)) (not (boolean? node-2))
                      (not (equal? (dir-digest node-1) (dir-digest node-2)))) #f
                      (let ((result
                             (let loop-1 ((n-1 node-1) (n-2 node-2))
                               (cond ((boolean? n-1) n-2)
                                     ((boolean? n-2) n-1)
                                     ((not (sync-pair? n-1)) n-2)
                                     ((not (sync-pair? n-2)) n-1)
                                     (else (let ((n-3 (dir-merge n-1 n-2)))
                                             (let loop-2 ((n-3 n-3) (keys (dir-all n-3)))
                                               (if (null? keys) n-3
                                                   (let* ((k (car keys))
                                                          (v-1 (dir-get n-1 k))
                                                          (v-2 (dir-get n-2 k))
                                                          (v-3 (loop-1 v-1 v-2)))
                                                     (loop-2 (dir-overlay n-3 k v-3)
                                                             (cdr keys)))))))))))
                        (begin (r-write! path result) #t))))))

       (define (record-infer! path)
         "Using the existing subdirectory structure, infer the
         original hash tree that would have generated those (potentially
         pruned) subdirectories.

         > path (list sym|vec): path from the record root to the target directory
         < return (bool): boolean indicating success of the operation"
         (let ((path (map key->bytes path)))
           (r-write! path
                     (let recurse ((node (r-read path)))
                       (if (or (not (sync-pair? (node->obj node))) (dir-overlay? node)) node
                           (let loop ((node node) (keys (dir-all node)))
                             (if (null? keys) (dir-align node)
                                 (loop (dir-set node (car keys)
                                                (recurse (dir-get node (car keys))))
                                       (cdr keys)))))) #t)
           #t))

       (define-macro (trace name)
         (let ((name-new (gensym)))
           `(begin
              (define ,name-new ,name)
              (define (,name . args)
                (display ">> ")
                (print (cons ,name args))
                (apply ,name-new args)))))

       (define trace-all
         (cons 'begin
               (let loop ((env (map car (curlet))) (ls '()))
                 (if (null? env) ls
                     (if (or (not (procedure? (eval (car env))))
                             (eq? (car env) 'print))
                         (loop (cdr env) ls)
                         (loop (cdr env) (cons `(trace ,(car env)) ls)))))))

       ;; (eval trace-all)

       (lambda (function)
         (case function
           ((get) record-get)
           ((equal?) record-equal?)
           ((equivalent?) record-equivalent?)
           ((set!) record-set!)
           ((copy!) record-copy!)
           ((merge!) record-merge!)
           ((slice!) record-slice!)
           ((prune!) record-prune!)
           ((infer!) record-infer!)
           ((serialize) record-serialize)
           ((deserialize!) record-deserialize!)
           (else (error 'unknown-function "Function not found"))))))

  (define transition-function
    `(lambda (*sync-state* query)
       (if (and (pair? query) (eq? (car query) '*administer*)
                (equal? (sync-hash (expression->byte-vector (cadr query)))
                        ,secret-hash))
           (let ((result (eval (caddr query))))
             (cons result *sync-state*))
           (let* ((state (sync-cdr *sync-state*))
                  (get (lambda () state))
                  (set (lambda (x) (set! state x)))
                  (record ((eval ,record-new) get set))
                  (result ((eval ,control) record ,secret-hash query)))
             (cons result (sync-cons (sync-car *sync-state*) state))))))

  (let* ((transition-bytes (expression->byte-vector transition-function))
         (state (sync-cdr *sync-state*))
         (get (lambda () state))
         (set (lambda (x) (set! state x)))
         (record ((eval record-new) get set)))
    ((record 'set!) '(record library record) record-new)
    (let loop ((scripts scripts))
      (if (null? scripts)
          (set! *sync-state* (sync-cons transition-bytes state))
          (begin ((eval (car scripts)) record)
                 (loop (cdr scripts))))))

  "Installed record interface")
