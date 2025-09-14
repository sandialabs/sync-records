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

       (define (key-bits key)
         (let loop-1 ((bytes (map (lambda (x) x) (sync-hash key))) (ret '()))
           (if (null? bytes) (reverse ret)
               (let* ((byte (car bytes))
                      (as-bits (lambda (byte) 
                                 (let loop-2 ((i 0) (bits '()))
                                   (if (< i -7) (reverse bits)
                                       (loop-2 (- i 1) (cons (logand (ash byte i) 1) bits)))))))
                 (loop-1 (cdr bytes) (append (as-bits byte) ret))))))

       (define (dir-new)
         (sync-null))

       (define (dir-get node key)
         (let loop ((node node) (bits (key-bits key)))
           (cond ((sync-null? node) node)
                 ((sync-stub? node) node)
                 ((byte-vector? (sync-car node))
                  (if (equal? key (sync-car node)) (sync-cdr node) (sync-null)))
                 (else (if (zero? (car bits))
                           (loop (sync-car node) (cdr bits))
                           (loop (sync-cdr node) (cdr bits)))))))

       (define (dir-set node key value)
         (let loop-1 ((node node) (bits (key-bits key)) (depth 0))
           (if (or (sync-null? node) (sync-stub? node)) (sync-cons key value)
               (let ((left (sync-car node)) (right (sync-cdr node)))
                 (if (not (byte-vector? left))
                     (if (zero? (car bits))
                         (sync-cons (loop-1 left (cdr bits) (+ depth 1)) right)
                         (sync-cons left (loop-1 right (cdr bits) (+ depth 1))))
                     (if (equal? left key) (sync-cons key value)
                         (let loop-2 ((bits-new bits) (bits-old (list-tail (key-bits left) depth)))
                           (cond ((and (zero? (car bits-new)) (zero? (car bits-old)))
                                  (sync-cons (loop-2 (cdr bits-new) (cdr bits-old)) (sync-null)))
                                 ((and (not (zero? (car bits-new))) (not (zero? (car bits-old))))
                                  (sync-cons (sync-null) (loop-2 (cdr bits-new) (cdr bits-old))))
                                 ((and (zero? (car bits-new)) (not (zero? (car bits-old))))
                                  (sync-cons (sync-cons key value) node))
                                 ((and (not (zero? (car bits-new))) (zero? (car bits-old)))
                                  (sync-cons node (sync-cons key value)))
                                 (else (error 'logic-error "Missing conditions"))))))))))

       (define (dir-delete node key)
         (let loop ((node node) (bits (key-bits key)))
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

       (define (dir-digest node)
         (sync-digest node))

       (define (dir-slice node key)
         (let loop ((node node) (bits (key-bits key)))
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

       (define (dir-prune node key keep-key?)
         (let loop ((node node) (bits (key-bits key)))
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

       (define (dir-merge node-1 node-2)
         (let recurse ((node-1 node-1) (node-2 node-2))
           (cond ((and (sync-stub? node-1) (sync-stub? node-2)) node-1)
                 ((and (not (sync-stub? node-1)) (sync-stub? node-2)) node-1)
                 ((and (sync-stub? node-1) (not (sync-stub? node-2))) node-2)
                 ((and (sync-pair? node-1) (sync-pair? node-2))
                  (sync-cons (recurse (sync-car node-1) (sync-car node-2))
                             (recurse (sync-cdr node-1) (sync-cdr node-2))))
                 ((equal? node-1 node-2) node-1)
                 (else (error 'invalid-structure "Cannot merge incompatible structure")))))

       (define (dir-all node)
         (let recurse ((node node))
           (cond ((sync-null? node) '(() #t))
                 ((sync-stub? node) '(() #f))
                 (else (let ((left (sync-car node)) (right (sync-cdr node)))
                         (if (byte-vector? left) `((,left) #t)
                             (let ((all-l (recurse left)) (all-r (recurse right)))
                               `(,(append (car all-l) (car all-r))
                                 ,(and (cadr all-l) (cadr all-r))))))))))

       (define (dir-valid? node)
         (let ((new (let loop ((node node) (bits '()))
                      (if (or (sync-null? node) (sync-stub? node)) node
                          (let ((left (sync-car node)) (right (sync-cdr node)))
                            (if (byte-vector? left)
                                (let* ((bits-found (key-bits left))
                                       (prfx-length (- (length bits-found) (length bits)))
                                       (prfx (list-tail (reverse bits-found) prfx-length)))
                                  (if (equal? bits prfx) node (sync-null)))
                                (sync-cons (loop left (cons 0 bits))
                                           (loop right (cons 1 bits)))))))))
           (equal? node new)))

       (define (key->bytes key)
         (cond ((sync-node? key) (error 'invalid-type "Keys cannot be sync nodes"))
               ((byte-vector? key) (append #u(0) key))
               (else (append #u(1) (expression->byte-vector key)))))

       (define (bytes->key bytes)
         (case (bytes 0)
           ((0) (subvector bytes 1))
           ((1) (byte-vector->expression (subvector bytes 1)))
           (else (error 'invalid-type "Key type encoding not recognized"))))

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

       (define struct-tag (sync-cons (sync-null) (sync-null)))

       (define (struct? node)
         (and (sync-pair? node) (equal? (sync-car node) struct-tag)))

       (define (r-read path)
         (let loop ((node (root-get)) (path path))
           (cond ((sync-null? node) node)
                 ((sync-stub? node) node)
                 ((null? path) node)
                 (else (loop (dir-get node (car path)) (cdr path))))))

       (define* (r-write! path value)
         (if (not value) #f
             (root-set!
              (let loop ((node (root-get)) (path path))
                (if (null? path) value
                    (let* ((key (car path))
                           (node (if (sync-null? node) (dir-new) node))
                           (old (dir-get node key)))
                      (dir-set node key (loop old (cdr path)))))))))

       (define (r-valid? node)
         (let loop-1 ((node node))
           (cond ((sync-null? node) #t)
                 ((sync-stub? node) #t)
                 ((byte-vector? node) #t)
                 ((struct? node) #t)
                 ((not (dir-valid? node)) #f)
                 (else (let loop-2 ((keys (car (dir-all node))))
                         (if (null? keys) #t
                             (if (not (loop-2 (cdr keys))) #f
                                 (loop-1 (dir-get node (car keys))))))))))

       (define (obj->node obj)
         (cond ((sync-node? obj) obj)
               ((procedure? obj) (sync-cons struct-tag (obj)))
               ((byte-vector? obj) (append #u(0) obj)) 
               (else (append #u(1) (expression->byte-vector obj)))))

       (define (node->obj node)
         (cond ((byte-vector? node)
                (case (node 0)
                  ((0) (subvector node 1))
                  ((1) (byte-vector->expression (subvector node 1)))
                  (else (error 'invalid-type "Type encoding unrecognized"))))
               ((sync-null? node) node)
               ((sync-stub? node) node)
               ((sync-pair? node)
                (if (not (equal? (sync-car node) struct-tag)) node
                    (lambda () (sync-cdr node))))
               (else (error 'invalid-type "Invalid value type"))))

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
             - 'unknown type indicates the path has been cut"
         (let ((path (map key->bytes path)))
           (let ((obj (node->obj (r-read path))))
             (if (sync-node? obj)
                 (cond ((sync-null? obj) '(nothing ()))
                       ((sync-stub? obj) '(unknown ()))
                       (else (let ((all (dir-all obj)))
                               `(directory ,(map bytes->key (car all)) ,(cadr all)))))
                 (cond ((procedure? obj) `(structure ,(obj)))
                       (else `(object ,obj)))))))

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
             (cond ((and (byte-vector? val-1) (byte-vector? val-2))
                    (equal? val-1 val-2))
                   ((and (sync-node? val-1) (sync-node? val-2))
                    (sync-equivalent? val-1 val-2))
                   (else #f)))))

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
                     (let* ((h (if (sync-node? node) (sync-digest node) (sync-hash node)))
                            (id (sym (byte-vector->hex-string h))))
                       (cond ((tb id) id)
                             ((sync-null? node) id)
                             ((byte-vector? node) (set! (tb id) #t)
                              (set! ls (cons `(,id (c ,(byte-vector->hex-string node))) ls)) id)
                             ((sync-stub? node) (set! (tb id) #t)
                              (set! ls (cons `(,id (s ,(byte-vector->hex-string (sync-digest node)))) ls)) id)
                             (else (set! (tb id) #t)
                                   (set! ls (cons `(,id ,(recurse (sync-car node))
                                                        ,(recurse (sync-cdr node))) ls)) id))))
                   (let* ((counter 0)
                          (seen (hash-table))
                          (null (sym (byte-vector->hex-string (sync-digest (sync-null)))))
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
                  (if (null? path) (dir-new)
                      (let ((child (loop (dir-get node (car path)) (cdr path))))
                        (if (equal? child (dir-new)) (dir-delete node (car path))
                            (dir-set node (car path) child)))))))
             (let ((value (if (sync-node? value) (lambda () value) value)))
               (r-write! (map key->bytes path) (obj->node value)))))

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
           (r-write! path (r-read source))))

       (define (record-deserialize! path serialization)
         "Validate and write serialized data to the specified path

         > path (list sym|vec): path from the record root to the target location
         > serialization (exp): expression containing the serialized data
         < return (bool): boolean indicating success of the operation"
         (let ((path (map key->bytes path)))
           (let* ((proc (lambda (x)
                          (let ((k (car x)) (v (cadr x)))
                            (case (car v)
                              ((c) `(define ,k  ,(hex-string->byte-vector (cadr v))))
                              ((s) `(define ,k  ,(sync-stub (hex-string->byte-vector (cadr v)))))
                              (else `(define ,k (sync-cons ,(car v) ,(cadr v))))))))
                  (expr `(begin (define n-0 (sync-null))
                                ,@(map proc (reverse serialization))))
                  (node (eval expr)))
             (if (not (r-valid? node))
                 (error 'deserialization-failure "Invalid serialization expression")
                 (r-write! path node)))))
       
       (define* (record-prune! path subpath keep-key?)
         "Prune specified data from a directory while maintaining the
         original hashes. If executed on directory that has not been previously
         pruned or sliced, then the directory becomes an overlayed directory.

         > path (list sym|vec): path from the record root to the target directory 
         > subpath (exp): subpath from the target directory to target data 
         < return (bool): boolean indicating success of the operation"
         (let ((path (map key->bytes path)) (subpath (map key->bytes subpath)))
           (r-write! path (let loop ((node (r-read path)) (subpath subpath))
                            (if (or (sync-null? node) (null? subpath)) (sync-null)
                                (let ((child (loop (dir-get node (car subpath))
                                                   (cdr subpath))))
                                  (if (not (sync-null? child)) (dir-set node (car subpath) child)
                                      (dir-prune node (car subpath) keep-key?))))))))

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
                       (cond ((null? subpath) node)
                             ((byte-vector? node) node)
                             ((struct? node) node)
                             (else (let ((key (car subpath)))
                                     (dir-slice (dir-set node key (loop (dir-get node key) (cdr subpath)))
                                                key))))))))

       (define (record-merge! source path)
         "Recursively combine data from two equivalent directories.

         > source (list sym|vec): path from the record root to the source directory 
         > path (list sym|vec): path from the record root to the target directory 
         < return (bool): boolean indicating success of the operation"
         (let ((source (map key->bytes source)) (path (map key->bytes path)))
           (let ((node-1 (r-read source)) (node-2 (r-read path)))
             (if (or (sync-null? node-1) (not (sync-equivalent? node-1 node-2))) #f
                 (r-write! path (let loop-1 ((n-1 node-1) (n-2 node-2))
                                  (cond ((byte-vector? n-1) n-1)
                                        ((sync-null? n-1) n-2)
                                        ((sync-null? n-2) n-1)
                                        ((sync-stub? n-1) n-2)
                                        ((sync-stub? n-2) n-1)
                                        ((struct? n-1) n-1)
                                        (else (let ((n-3 (dir-merge n-1 n-2)))
                                                (let loop-2 ((n-3 n-3) (keys (car (dir-all n-3))))
                                                  (if (null? keys) n-3
                                                      (let* ((k (car keys))
                                                             (v-1 (dir-get n-1 k))
                                                             (v-2 (dir-get n-2 k))
                                                             (v-3 (loop-1 v-1 v-2)))
                                                        (loop-2 (dir-set n-3 k v-3)
                                                                (cdr keys))))))))))))))

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
                  (set (lambda (x) (set! state x) #t))
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
