(define-class (standard)
  ;; Standard class builds and manipulates sync objects generically.

  (define-method (make self class)
    ;; Build an uninitialized object shell from a define-class form.
    ;;   Args:
    ;;     class (list): define-class form.
    ;;   Returns:
    ;;     sync node: instance.
    (if (not (eq? (car class) 'define-class))
        (error 'make-error "Please load in a class definition"))

    (let* ((name (caadr class))
           (methods (let loop ((body (cddr class)) (methods '()))
                      (cond ((null? body) (reverse methods))
                            ((string? (car body)) (loop (cdr body) methods))
                            (else (if (not (eq? (caar body) 'define-method))
                                      (error 'component-error
                                             "Only 'define-method expressions are allowed within define-class"))
                                  (loop (cdr body)
                                        (cons `(,(caadar body) (lambda* ,(cdadar body) ,@(cddar body))) methods))))))
           (api (let ((proc (lambda (x) (append " " (symbol->string (car x))))))
                  (substring (apply append (map proc methods)) 1)))
           (description (append "--- Standard Class ---\n"
                                "Name: " (symbol->string name) "\n"
                                "Description: " (if (string? (caddr class)) (caddr class) "") "\n"
                                "Functions: " api "\n"
                                "-------------------------"))
           (err '(error 'function-error (append "Function not recognized: " (symbol->string arg))))
           (common `(((*name*) ,name) ((*api*) '(*name* *api* *class* ,@(map car methods))) ((*class*) ,class)))
           (prep (lambda (x) `((,(car x)) (lambda args
                                            (let ((res (apply ,(cadr x) (cons self args)))) res)))))
           (get '(lambda (node path)
                   (let loop ((node node) (path path))
                     (if (null? path) node
                         (if (zero? (car path))
                             (loop (sync-car node) (cdr path))
                             (loop (sync-cdr node) (cdr path)))))))
           (set '(lambda*
                  (arg-1 arg-2)
                  (set! state
                        (let loop ((node (self)) (path (if arg-2 arg-1 '())))
                          (if (null? path) (if arg-2 arg-2 arg-1)
                              (let ((node (if (sync-pair? node) node (sync-cons (sync-null) (sync-null)))))
                                (if (zero? (car path))
                                    (sync-cons (loop (sync-car node) (cdr path)) (sync-cdr node))
                                    (sync-cons (sync-car node) (loop (sync-cdr node) (cdr path)))))))) #t))
           (inner `(lambda (node)
                     (letrec* ((payload (sync-cdr node))
                               (input (let loop ((expr (byte-vector->expression (sync-car payload))) (nodes (sync-cdr payload)) (out '()))
                                        (if (null? (cdr expr)) (append (reverse (car expr)) out)
                                            (loop (cdr expr) (sync-cdr nodes)
                                                  (append (cons (sync-car nodes) (reverse (car expr))) out)))))
                               (state (cadr input))
                               (self (lambda* (arg)
                                              (set! (setter self) ,set)
                                              (cond ((not arg) state)
                                                    ((list? arg) (,get state arg))
                                                    (else (case arg
                                                            ,@common
                                                            ,@(map prep methods)
                                                            (else ,err))))))
                               (result (apply (self (car input)) (cddr input))))
                       (sync-cons state (cond ((sync-node? result) result)
                                              ((byte-vector? result) (append #u(0) result))
                                              (else (append #u(1) (expression->byte-vector result))))))))
           (outer `(lambda (state)
                     (define* (,name func) ,description
                       (cond ((not func) state)
                             ((list? func) (,get state func)) 
                             (else (case func
                                     ,@common
                                     (else (lambda args
                                             (let* ((payload (let loop ((in (cons func (cons state args))) (ls '()) (expr '()) (nodes (sync-null)))
                                                             (cond ((null? in) (sync-cons (expression->byte-vector (cons ls expr)) nodes))
                                                                   ((sync-node? (car in)) (loop (cdr in) '() (cons ls expr) (sync-cons (car in) nodes)))
                                                                   (else (loop (cdr in) (cons (car in) ls) expr nodes)))))
                                                    (result (sync-eval (sync-cons (expression->byte-vector ',inner) payload) #t ',name func))
                                                    (state-new (sync-car result)) 
                                                    (output (sync-cdr result)))
                                               (set! state state-new)
                                               (if (sync-node? output) output
                                                   (case (byte-vector-ref output 0)
                                                     ((0) (subvector output 1))
                                                     ((1) (byte-vector->expression (subvector output 1)))
                                                     (else (error 'encoding-error "Unknown standard output tag")))))))))))))
           (object ((eval outer) (sync-cons (expression->byte-vector outer) (sync-null)))))
      (object)))

  (define-method (init self class . init)
    ;; Build an object shell and run its *init* method with args.
    ;;   Args:
    ;;     class (list): define-class form.
    ;;     init (list): constructor args.
    ;;   Returns:
    ;;     sync node: initialized instance.
    (let ((object (sync-eval ((self 'make) class) #f)))
      (if (member '*init* (object '*api*))
          (apply (object '*init*) init))
      (object)))

  (define-method (deep-get self object path)
    ;; Get value at path across nested nodes.
    ;;   Args:
    ;;     object (sync node): target node.
    ;;     path (list): path segments.
    ;;   Returns:
    ;;     any: value or nested node.
    (let ((object (sync-eval object #f)))
      (if (null? path) (object)
          (let ((node ((object 'get) (car path))))
            (cond ((equal? node '(nothing)) '(nothing))
                  ((equal? node '(unknown)) '(unknown))
                  ((not (sync-node? node))
                   (if (null? (cdr path)) node
                       (error 'path-error "Cannot continue deep-get through a non-object value")))
                  ((sync-stub? node) '(unknown))
                  ((null? (cdr path)) node)
                  ((and (sync-pair? node) (byte-vector? (sync-car node)))
                   ((self 'deep-get) node (cdr path)))
                  (else (error 'path-error "Cannot continue deep-get through a non-object value")))))))

  (define-method (deep-set! self object path value)
    ;; Set value at path across nested nodes.
    ;;   Args:
    ;;     object (sync node): target node.
    ;;     path (list): path segments.
    ;;     value (any): value to set.
    ;;   Returns:
    ;;     sync node: rebuilt node.
    (let ((object (sync-eval object #f)))
      (if (= (length path) 1)
          (begin ((object 'set!) (car path) value)
                 (object))
          (let ((child ((self 'deep-set!) ((object 'get) (car path)) (cdr path) value)))
            ((object 'set!) (car path) child)
            (object)))))

  (define-method (deep-slice! self object path)
    ;; Slice node to retain proof along path.
    ;;   Args:
    ;;     object (sync node): target node.
    ;;     path (list): path segments.
    ;;   Returns:
    ;;     sync node: rebuilt node.
    (let ((object (sync-eval object #f)))
      (if (= (length path) 1)
          (begin ((object 'slice!) (car path))
                 (object))
          (let* ((child ((object 'get) (car path)))
                 (result ((self 'deep-slice!) child (cdr path)))
                 (digest (sync-digest child)))
            (if (not (equal? (sync-digest result) digest))
                (error 'digest-error "Slice operation caused digest change"))
            ((object 'set!) (car path) result)
            ((object 'slice!) (car path))
            (object)))))

  (define-method (deep-prune! self object path)
    ;; Prune node to remove proof along path.
    ;;   Args:
    ;;     object (sync node): target node.
    ;;     path (list): path segments.
    ;;   Returns:
    ;;     sync node: rebuilt node.
    (let ((object (sync-eval object #f)))
      (if (= (length path) 1)
          (begin ((object 'prune!) (car path))
                 (object))
          (let ((child ((object 'get) (car path))))
            (if (not (and (sync-node? child) (sync-pair? child))) (object)
                (let* ((result ((self 'deep-prune!) child (cdr path)))
                       (digest (sync-digest child)))
                  (if (not (equal? (sync-digest result) digest))
                      (error 'digest-error "Prune operation caused digest change"))
                  (if (sync-stub? result)
                      ((object 'prune!) (car path))
                      ((object 'set!) (car path) result))
                  (object)))))))

  (define-method (deep-merge! self object-source object-target . path-rest)
    ;; Merge equivalent nodes by digest.
    ;;   Args:
    ;;     object-source (sync node): source node.
    ;;     object-target (sync node): target node.
    ;;     path (list): optional path inside target to merge into.
    ;;   Returns:
    ;;     sync node: merged node.
    (let ((path (if (null? path-rest) '() (car path-rest)))
          (merge-nodes
           (lambda (object-source object-target)
             (if (not (equal? (sync-digest object-source) (sync-digest object-target)))
                 (error 'node-error "Cannot merge non-equivalent objects")
                 (let recurse ((node-1 object-source) (node-2 object-target))
                   (cond ((sync-null? node-1) node-1)
                         ((byte-vector? node-1) node-1)
                         ((sync-stub? node-1) node-2)
                         ((sync-stub? node-2) node-1)
                         (else (sync-cons (recurse (sync-car node-1) (sync-car node-2))
                                          (recurse (sync-cdr node-1) (sync-cdr node-2))))))))))
      (if (null? path) (merge-nodes object-source object-target)
          ((self 'deep-set!) object-target path
           (merge-nodes object-source ((self 'deep-get) object-target path))))))

  (define-method (deep-copy! self object path-source path-target)
    ;; Copy value from source path to target path.
    ;;   Args:
    ;;     object (sync node): target node.
    ;;     path-source (list): source path.
    ;;     path-target (list): target path.
    ;;   Returns:
    ;;     sync node: rebuilt node.
    ((self 'deep-set!) object path-target ((self 'deep-get) object path-source)))

  (define-method (deep-call self object path function)
    ;; Call function on node at path and return the callback result.
    ;;   Args:
    ;;     object (sync node): target node.
    ;;     path (list): path segments.
    ;;     function (procedure): callback on the loaded object at the target path.
    ;;   Returns:
    ;;     any: callback result.
    (let ((object (sync-eval object #f)))
      (if (null? path)
          ((eval function) object)
          (let* ((child ((object 'get) (car path)))
                 (result ((self 'deep-call) child (cdr path) function)))
            result))))

  (define-method (deep-call! self object path function)
    ;; Call function on node at path and rebuild the resulting node state.
    ;;   Args:
    ;;     object (sync node): target node.
    ;;     path (list): path segments.
    ;;     function (procedure): callback on the loaded object at the target path.
    ;;   Returns:
    ;;     sync node: rebuilt node.
    (let ((object (sync-eval object #f)))
      (if (null? path)
          (begin
            ((eval function) object)
            (object))
          (let* ((child ((object 'get) (car path)))
                 (child ((self 'deep-call!) child (cdr path) function)))
            ((object 'set!) (car path) child)
            (object)))))

  (define-method (serialize self node query)
    ;; Serialize node with a traversal query into compact form.
    ;;   Args:
    ;;     node (sync node): root node.
    ;;     query (procedure/#f): traversal callback, or #f to serialize the full subtree.
    ;;   Returns:
    ;;     list: serialization list.
    (let* ((ls '())
           (tab (hash-table))
           (add (lambda (x y z)
                  (let ((init (if (not (tab x)) (cons y z) (cons (if y y (car (tab x))) (if z z (cdr (tab x)))))))
                    (set! (tab x) (cons (if y y (car init)) (if z z (cdr init)))))))
           (~sync-cons (lambda (y z) (let ((x (sync-cons y z))) (add x y z) x)))
           (~sync-car (lambda (x) (let ((y (sync-car x))) (add x y #f) y)))
           (~sync-cdr (lambda (x) (let ((z (sync-cdr x))) (add x #f z) z)))
           (~ (if query
                  (with-let (sublet (rootlet) '*query* query '*node* node
                                    'sync-cons ~sync-cons 'sync-car ~sync-car 'sync-cdr ~sync-cdr)
                            (letrec* ((rootlet curlet)
                                      (sync-eval (lambda* (x . rest) ((eval (byte-vector->expression (sync-car x))) x))))
                              ((eval *query*) *node*)))
                  #f))
           (tree (if (not query) node
                     (let recurse ((node node))
                       (let ((left (if (tab node) (car (tab node)) #f))
                             (right (if (tab node) (cdr (tab node)) #f)))
                         (sync-cons (cond ((not left) (sync-cut (sync-car node)))
                                          ((not (sync-pair? left)) left)
                                          (else (recurse left)))
                                    (cond ((not right) (sync-cut (sync-cdr node)))
                                          ((not (sync-pair? right)) right)
                                          (else (recurse right))))))))
           (sym (lambda (x) (string->symbol (append "n-" x))))
           (~ (let recurse ((node tree) (tb (hash-table)))
                (let* ((h (sync-digest node))
                       (id (sym (byte-vector->hex-string h))))
                  (cond ((tb id) id)
                        ((sync-null? node) id)
                        ((byte-vector? node) (set! (tb id) #t)
                         (set! ls (cons `(,id (c ,node)) ls)) id)
                        ((sync-stub? node) (set! (tb id) #t)
                         (set! ls (cons `(,id (s ,(sync-digest node))) ls)) id)
                        (else (set! (tb id) #t)
                              (set! ls (cons `(,id ,(recurse (sync-car node) tb)
                                                   ,(recurse (sync-cdr node) tb)) ls)) id)))))
           (counter 0)
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
      (map (lambda (x) (compact (map shorten x))) ls)))

  (define-method (deserialize self serialization)
    ;; Deserialize a serialization list into a sync node.
    ;;   Args:
    ;;     serialization (list): serialization list.
    ;;   Returns:
    ;;     sync node: deserialized node.
    (let* ((proc (lambda (x)
                   (let ((k (car x)) (v (cadr x)))
                     (case (car v)
                       ((c) `(define ,k  ,(cadr v)))
                       ((s) `(define ,k  ,(sync-stub (cadr v))))
                       (else `(define ,k (sync-cons ,(car v) ,(cadr v))))))))
           (expr `(begin (define n-0 (sync-null))
                         ,@(map proc (reverse serialization)))))
      (eval expr))))
