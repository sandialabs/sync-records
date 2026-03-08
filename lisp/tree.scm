(define-class (tree)
  ;; Tree class provides a hashed key-value directory with Merkle-style nodes.

  (define-method (~key-bits self key)
    ;; Convert key to a list of bits derived from its hash.
    ;;   Args:
    ;;     key (any): lookup key.
    ;;   Returns:
    ;;     list: bit list (0/1) for the key hash.
    (let loop-1 ((bytes (map (lambda (x) x) (sync-hash key))) (ret '()))
      (if (null? bytes) (reverse ret)
          (let* ((byte (car bytes))
                 (as-bits (lambda (byte) 
                            (let loop-2 ((i 0) (bits '()))
                              (if (< i -7) (reverse bits)
                                  (loop-2 (- i 1) (cons (logand (ash byte i) 1) bits)))))))
            (loop-1 (cdr bytes) (append (as-bits byte) ret))))))

  (define-method (~dir-new self)
    ;; Create an empty directory node.
    ;;   Returns:
    ;;     sync node: empty directory node.
    (sync-null))

  (define-method (~dir-get self node key)
    ;; Fetch value for key within a directory node.
    ;;   Args:
    ;;     node (sync node): directory root node.
    ;;     key (any): lookup key.
    ;;   Returns:
    ;;     sync node: value node, sync-null, or stub.
    (let loop ((node node) (bits ((self '~key-bits) key)))
      (cond ((sync-null? node) node)
            ((sync-stub? node) node)
            ((byte-vector? (sync-car node))
             (if (equal? key (sync-car node)) (sync-cdr node) (sync-null)))
            (else (if (zero? (car bits))
                      (loop (sync-car node) (cdr bits))
                      (loop (sync-cdr node) (cdr bits)))))))

  (define-method (~dir-set self node key value)
    ;; Set key to value within a directory node.
    ;;   Args:
    ;;     node (sync node): directory root node.
    ;;     key (any): lookup key.
    ;;     value (sync node): value to set.
    ;;   Returns:
    ;;     sync node: updated directory node.
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

  (define-method (~dir-delete self node key)
    ;; Delete key from directory node and collapse empty branches.
    ;;   Args:
    ;;     node (sync node): directory root node.
    ;;     key (any): lookup key.
    ;;   Returns:
    ;;     sync node: updated directory node.
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

  (define-method (~dir-digest self node)
    ;; Compute digest of a directory node.
    ;;   Args:
    ;;     node (sync node): directory root node.
    ;;   Returns:
    ;;     byte-vector: digest of node.
    (sync-digest node))

  (define-method (~dir-slice self node key)
    ;; Slice directory to include only path to key with cuts elsewhere.
    ;;   Args:
    ;;     node (sync node): directory root node.
    ;;     key (any): lookup key.
    ;;   Returns:
    ;;     sync node: sliced directory node.
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

  (define-method (~dir-prune self node key keep-key?)
    ;; Prune subtree at key, optionally keeping the key itself.
    ;;   Args:
    ;;     node (sync node): directory root node.
    ;;     key (any): lookup key.
    ;;     keep-key? (boolean): whether to keep the key node.
    ;;   Returns:
    ;;     sync node: pruned directory node.
    (let loop ((node node) (bits ((self '~key-bits) key)))
      (if (or (sync-null? node) (sync-stub? node)) node
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

  (define-method (~dir-merge self node-1 node-2)
    ;; Merge two directory nodes with compatible structure.
    ;;   Args:
    ;;     node-1 (sync node): directory node.
    ;;     node-2 (sync node): directory node.
    ;;   Returns:
    ;;     sync node: merged directory node.
    (let recurse ((node-1 node-1) (node-2 node-2))
      (cond ((and (sync-stub? node-1) (sync-stub? node-2)) node-1)
            ((and (not (sync-stub? node-1)) (sync-stub? node-2)) node-1)
            ((and (sync-stub? node-1) (not (sync-stub? node-2))) node-2)
            ((and (sync-pair? node-1) (sync-pair? node-2))
             (sync-cons (recurse (sync-car node-1) (sync-car node-2))
                        (recurse (sync-cdr node-1) (sync-cdr node-2))))
            ((equal? node-1 node-2) node-1)
            (else (error 'invalid-structure "Cannot merge incompatible structure")))))

  (define-method (~dir-all self node)
    ;; Collect all keys in a directory node and whether it is fully known.
    ;;   Args:
    ;;     node (sync node): directory root node.
    ;;   Returns:
    ;;     list: (keys-list known?) for this subtree.
    (let recurse ((node node))
      (cond ((sync-null? node) '(() #t))
            ((sync-stub? node) '(() #f))
            (else (let ((left (sync-car node)) (right (sync-cdr node)))
                    (if (byte-vector? left) `((,left) #t)
                        (let ((all-l (recurse left)) (all-r (recurse right)))
                          `(,(append (car all-l) (car all-r))
                            ,(and (cadr all-l) (cadr all-r))))))))))

  (define-method (~dir-valid? self node)
    ;; Validate that directory node matches key prefixes.
    ;;   Args:
    ;;     node (sync node): directory root node.
    ;;   Returns:
    ;;     boolean: True/False for prefix validity.
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

  (define-method (~key->bytes self key)
    ;; Encode a key to a tagged byte-vector.
    ;;   Args:
    ;;     key (any): lookup key.
    ;;   Returns:
    ;;     byte-vector: tagged encoding of key.
    (cond ((sync-node? key) (error 'invalid-type "Keys cannot be sync nodes"))
          ((byte-vector? key) (append #u(0) key))
          (else (append #u(1) (expression->byte-vector key)))))

  (define-method (~bytes->key self bytes)
    ;; Decode a tagged byte-vector into a key.
    ;;   Args:
    ;;     bytes (byte-vector): encoded key bytes.
    ;;   Returns:
    ;;     key: decoded key value.
    (case (bytes 0)
      ((0) (subvector bytes 1))
      ((1) (byte-vector->expression (subvector bytes 1)))
      (else (error 'invalid-type "Key type encoding not recognized"))))

  (define-method (~struct-tag self)
    ;; Return the tag used to identify embedded structure nodes.
    ;;   Returns:
    ;;     sync node: struct tag node.
    (sync-cons (sync-null) (sync-null)))

  (define-method (~struct? self node)
    ;; Check whether node is a tagged structure wrapper.
    ;;   Args:
    ;;     node (sync node): candidate node.
    ;;   Returns:
    ;;     boolean: True/False if node is a struct wrapper.
    (and (sync-pair? node) (equal? (sync-car node) ((self '~struct-tag)))))

  (define-method (~r-read self path)
    ;; Read raw node at path without decoding.
    ;;   Args:
    ;;     path (list of byte-vectors): path segments.
    ;;   Returns:
    ;;     sync node: raw node at path.
    (let loop ((node (self '(1))) (path path))
      (cond ((sync-null? node) node)
            ((sync-stub? node) node)
            ((null? path) node)
            (else (loop ((self '~dir-get) node (car path)) (cdr path))))))

  (define-method (~r-write! self path value)
    ;; Write raw node value at path, creating directory nodes as needed.
    ;;   Args:
    ;;     path (list of byte-vectors): path segments.
    ;;     value (sync node): value to set.
    ;;   Returns:
    ;;     boolean: #t after writing raw node.
    (set! (self '(1))
          (let loop ((node (self '(1))) (path path))
            (if (null? path) value
                (let* ((key (car path))
                       (node (if (sync-null? node) ((self '~dir-new)) node))
                       (old ((self '~dir-get) node key)))
                  ((self '~dir-set) node key (loop old (cdr path))))))))

  (define-method (obj->node self obj)
    ;; Encode an object into a sync node representation.
    ;;   Args:
    ;;     obj (any): value to encode.
    ;;   Returns:
    ;;     sync node: encoded value node.
    (cond ((sync-node? obj) obj)
          ((procedure? obj) (sync-cons ((self '~struct-tag)) (obj)))
          ((byte-vector? obj) (append #u(0) obj)) 
          (else (append #u(1) (expression->byte-vector obj)))))

  (define-method (node->obj self node)
    ;; Decode a sync node into an object.
    ;;   Args:
    ;;     node (sync node): encoded node.
    ;;   Returns:
    ;;     any: decoded value or thunk for struct.
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

  (define-method (get self path)
    ;; Get value at path, decoding node types and directory info.
    ;;   Args:
    ;;     path (list of keys): path segments.
    ;;   Returns:
    ;;     any: value, '(nothing), '(unknown), or '(directory ((key type) ...) known?).
    (let ((path (map (self '~key->bytes) path)))
      (let ((obj ((self 'node->obj) ((self '~r-read) path))))
        (if (sync-node? obj)
            (cond ((sync-null? obj) '(nothing))
                  ((sync-stub? obj) '(unknown))
                  (else (let ((all ((self '~dir-all) obj)))
                          `(directory ,(map (lambda (k)
                                              `(,((self '~bytes->key) k)
                                                ,(let ((child ((self '~dir-get) obj k)))
                                                   (cond (((self '~struct?) child) 'object)
                                                         ((sync-node? child) (if (sync-stub? child) 'unknown 'directory))
                                                         (else 'value)))))
                                            (car all))
                                      ,(cadr all)))))
            (cond ((procedure? obj) (obj))
                  (else obj))))))

  (define-method (equal? self source path)
    ;; Compare raw nodes at two paths for exact equality.
    ;;   Args:
    ;;     source (list of keys): source path.
    ;;     path (list of keys): target path.
    ;;   Returns:
    ;;     boolean: True/False for exact node equality.
    (let ((source (map (self '~key->bytes) source)) (path (map (self '~key->bytes) path)))
      (equal? ((self '~r-read) source) ((self '~r-read) path))))

  (define-method (equivalent? self source path)
    ;; Compare raw nodes at two paths for digest-equivalence.
    ;;   Args:
    ;;     source (list of keys): source path.
    ;;     path (list of keys): target path.
    ;;   Returns:
    ;;     boolean: True/False for digest-equivalence.
    (let ((source (map (self '~key->bytes) source)) (path (map (self '~key->bytes) path)))
      (let ((val-1 ((self '~r-read) source)) (val-2 ((self '~r-read) path)))
        (cond ((and (byte-vector? val-1) (byte-vector? val-2))
               (equal? val-1 val-2))
              ((and (sync-node? val-1) (sync-node? val-2))
               (equal? (sync-digest val-1) (sync-digest val-2)))
              (else #f)))))

  (define-method (set! self path value)
    ;; Set value at path, handling special directory/unknown/nothing cases.
    ;;   Args:
    ;;     path (list of keys): path segments.
    ;;     value (any): value to set.
    ;;   Returns:
    ;;     boolean: #t after mutation.
    (cond ((equal? value '(unknown))
           (error 'value-error "Value conflicts with key expression '(unknown)"))
          ((and (list? value) (not (null? value)) (eq? (car value) 'directory))
           (error 'value-error "Value resembles key expression pattern '(directory ..)"))
          ((or (procedure? value) (macro? value))
           (error 'value-error "Cannot write function or macro values into tree data"))
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

  (define-method (copy! self source path)
    ;; Copy raw node from source path to target path.
    ;;   Args:
    ;;     source (list of keys): source path.
    ;;     path (list of keys): target path.
    ;;   Returns:
    ;;     boolean: #t after copy.
    (let ((source (map (self '~key->bytes) source)) (path (map (self '~key->bytes) path)))
      ((self '~r-write!) path ((self '~r-read) source))))

  (define-method (prune! self path keep-key?)
    ;; Prune subtree at path, optionally keeping the key node.
    ;;   Args:
    ;;     path (list of keys): path segments.
    ;;     keep-key? (boolean): whether to keep the key node.
    ;;   Returns:
    ;;     boolean: #t after prune.
    (let ((path (map (self '~key->bytes) path)))
      (set! (self '(1))
            (let loop ((node (self '(1))) (path path))
              (cond ((sync-null? node) node)
                    ((null? path) (sync-cut node))
                    (else (let ((child (loop ((self '~dir-get) node (car path)) (cdr path))))
                            (if (and (not (sync-stub? child)) (not (sync-null? child)))
                                ((self '~dir-set) node (car path) child)
                                ((self '~dir-prune) node (car path) keep-key?)))))))))

  (define-method (slice! self path)
    ;; Slice tree to include only nodes along path.
    ;;   Args:
    ;;     path (list of keys): path segments.
    ;;   Returns:
    ;;     boolean: #t after slice.
    (let ((path (map (self '~key->bytes) path)))
      (set! (self '(1))
            (let loop ((node (self '(1))) (path path))
              (cond ((null? path) node)
                    ((byte-vector? node) node)
                    (((self '~struct?) node) node)
                    (else (let ((key (car path)))
                            ((self '~dir-slice) ((self '~dir-set) node key (loop ((self '~dir-get) node key) (cdr path)))
                             key))))))))

  (define-method (merge! self other)
    ;; Merge another equivalent tree into this one.
    ;;   Args:
    ;;     other (tree): other tree.
    ;;   Returns:
    ;;     boolean: #t on success, #f if not mergeable.
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

  (define-method (valid? self)
    ;; Validate internal directory structure of the whole tree.
    ;;   Returns:
    ;;     boolean: True/False if tree structure is valid.
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
