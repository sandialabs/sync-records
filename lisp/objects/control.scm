;; authentication options
;; - have some kind of configurable system within the query handler
;; - wrap in some kind of master authentication system
;;   - could bake into the API somehow
;; - for every query
;; - ask authenticator if allowed, get #t/#f
;; - maybe all local comes with authentication?
;; - that just sounds like wrapping it into the functionality
;; - why not?
;; - implies we just have one interface or ledger, basically

(lambda (secret)
  (define transition-function
    '(lambda (*sync-state* query)
       (let* ((control-node (sync-car *sync-state*))
              (node-1 (sync-cdr *sync-state*))
              (node-10 (sync-car node-1))
              (node-11 (sync-cdr node-1))
              (secret-node (sync-car node-11))
              (root-node (sync-cdr node-11))
              (root ((eval (byte-vector->expression (sync-car root-node))) root-node)))

         (define (authenticate secret)
           (let ((secret-hash secret-node))
             (if (not (equal? (sync-hash (expression->byte-vector secret)) secret-hash))
                 (error 'authentication-failure "Could not identify as admin"))))

         (define (update! result)
           (set! *sync-state* (sync-cons control-node (sync-cons node-10 (sync-cons secret-node (root)))))
           result)

         (define (control-eval secret expression) 
           (authenticate secret)
           (eval expression))

         (define (control-step secret)
           (authenticate secret)
           (let* ((step-handler (eval (byte-vector->expression (sync-car node-10)))))
             (update! (step-handler root secret))))

         (define (control-call secret function)
           (authenticate secret)
           (update! ((eval function) root)))

         (define (control-query query)
           (let* ((query-handler (eval (byte-vector->expression (sync-cdr node-10)))))
             (update! (query-handler root query))))

         (define (control-set key value)
           (authenticate secret)
           (let* ((node-10 (sync-car node-1))
                  (step-node (if (eq? key 'step) (expression->byte-vector value) (sync-car node-10)))
                  (query-node (if (eq? key 'query) (expression->byte-vector value) (sync-cdr node-10)))
                  (secret-node (if (eq? key 'secret) (sync-hash (expression->byte-vector value)) (sync-car node-11)))
                  (root-node (root)))
             (set! *sync-state* (sync-cons control-node (sync-cons (sync-cons step-node query-node)
                                                                   (sync-cons secret-node root-node))))))

         (cons (case (car query)
                 ((*eval*) (apply control-eval (cdr query)))
                 ((*call*) (apply control-call (cdr query)))
                 ((*step*) (apply control-step (cdr query)))
                 ((*set-secret*) (apply control-set (append '(secret) (cdr query))))
                 ((*set-step*) (apply control-set (append '(step) (cdr query))))
                 ((*set-query*) (apply control-set (append '(query) (cdr query))))
                 (else (control-query query)))
               *sync-state*))))

  (define root-code
    '(lambda (state)
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
                 ((byte-vector? (sync-car node))
                  (if (equal? key (sync-car node)) (sync-cdr node) (sync-null)))
                 (else (if (zero? (car bits))
                           (loop (sync-car node) (cdr bits))
                           (loop (sync-cdr node) (cdr bits)))))))

       (define (dir-set node key value)
         (let loop-1 ((node node) (bits (key-bits key)) (depth 0))
           (if (sync-null? node) (sync-cons key value)
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
           (if (sync-null? node) (sync-null)
               (let ((left (sync-car node)) (right (sync-cdr node)))
                 (if (byte-vector? left)
                     (if (equal? key left) (sync-null) node)
                     (let ((left (if (zero? (car bits)) (loop left (cdr bits)) left))
                           (right (if (zero? (car bits)) right (loop right (cdr bits)))))
                       (cond ((and (sync-null? left) (sync-null? right)) (sync-null))
                             ((and (sync-null? left) (sync-pair? right) (byte-vector? (sync-car right))) right)
                             ((and (sync-null? right) (sync-pair? left) (byte-vector? (sync-car left))) left)
                             (else (sync-cons left right)))))))))

       (define (dir-all node)
         (let recurse ((node node))
           (if (sync-null? node) '()
               (let ((left (sync-car node)) (right (sync-cdr node)))
                 (if (byte-vector? left) `(,left)
                     (append (recurse left) (recurse right)))))))

       (define struct-tag (sync-cons (sync-null) (sync-null)))

       (define (node-get path)
         (let loop ((node (sync-cdr state)) (path path))
           (cond ((sync-null? node) node)
                 ((null? path) node)
                 (else (loop (dir-get node (car path)) (cdr path))))))

       (define (node-set! path value)
         (let ((data (let loop ((node (sync-cdr state)) (path path))
                       (if (null? path) value
                           (let* ((key (car path))
                                  (node (if (sync-null? node) (dir-new) node))
                                  (old (dir-get node key)))
                             (dir-set node key (loop old (cdr path))))))))
           (set! state (sync-cons (sync-car state) data)) #t))

       (define (node-delete! path)
         (let ((data (let loop ((node (sync-cdr state)) (path path))
                       (if (null? path) (dir-new)
                           (let ((child (loop (dir-get node (car path)) (cdr path))))
                             (if (equal? child (dir-new)) (dir-delete node (car path))
                                 (dir-set node (car path) child)))))))
           (set! state (sync-cons (sync-car state) data)) #t))

       (define (root-get path)
         (let ((node (node-get (map expression->byte-vector path))))
           (cond ((byte-vector? node)
                  (case (node 0)
                    ((0) `(content ,(subvector node 1)))
                    ((1) `(content ,(byte-vector->expression (subvector node 1))))))
                 ((sync-null? node) '(nothing))
                 ((equal? (sync-car node) struct-tag) `(content ,(sync-cdr node)))
                 (else `(directory ,(map byte-vector->expression (dir-all node)))))))

       (define (root-set! path value)
         (let ((path (map expression->byte-vector path)))
           (case (car value)
             ((nothing) (node-delete! path))
             ((content) (node-set! path (cond ((sync-node? (cadr value)) (sync-cons struct-tag (cadr value)))
                                              ((byte-vector? (cadr value)) (append #u(0) (cadr value)))
                                              (else (append #u(1) (expression->byte-vector (cadr value)))))))
             (else (error 'invalid-content "Content type not recognized")))))

       (define (root-copy! source target)
         (node-set! (map expression->byte-vector target)
                    (node-get (map expression->byte-vector source))))

       (define (root-equal? source target)
         (let ((source-node (node-get (map expression->byte-vector source)))
               (target-node (node-get (map expression->byte-vector target))))
           (equal? source-node target-node)))

       (define (root-equivalent? source target)
         (let ((source-node (node-get (map expression->byte-vector source)))
               (target-node (node-get (map expression->byte-vector target))))
           (if (byte-vector? source-node) (equal? source-node target-node)
               (equal? (sync-digest source-node) (sync-digest target-node)))))

       (define (print . exprs)
         (let loop ((exprs exprs))
           (if (null? exprs) (newline)
               (begin (display (car exprs)) (display " ") (loop (cdr exprs))))))

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
       
       (lambda*
        (function)
        (if (not function) state
            (case function
              ((get) root-get)
              ((set!) root-set!)
              ((copy!) root-copy!)
              ((equal?) root-equal?)
              ((equivalent?) root-equivalent?)
              (else (error 'unimplemented-method "Basic record does not implement method")))))))

  (define step
    '(lambda (root secret)
       (let* ((less? (lambda (x y)
                       (cond ((and (number? x) (number? y)) (< x y))
                             ((and (number? x) (not (number? y))) #t)
                             ((and (not (number? x)) (number? y)) #f)
                             (else (string<=? (symbol->string x) (symbol->string y))))))
              (names (cadr ((root 'get) '(control step)))))
         (let loop ((names (sort! names less?)) (rets '()))
           (if (null? names) (reverse rets)
               (let ((ret (sync-call `(*call* ,secret
                                              (lambda (root)
                                                (let* ((path '(control step ,(car names)))
                                                       (expr (cadr ((root 'get) path))))
                                                  ((eval expr) root)))) #t)))
                 (loop (cdr names) (cons `(,(car names) ,ret) rets))))))))

  (define query
    '(lambda (root query)
       (let ((result ((root 'get) `(control endpoint ,(car query)))))
         (if (eq? (car result) 'nothing)
             (error 'unknown-function "Function not found")
             (apply (eval (cadr result)) (cons root (cdr query)))))))

  (let ((control-node (expression->byte-vector transition-function))
        (secret-node (sync-hash (expression->byte-vector secret)))
        (step-node (expression->byte-vector step))
        (query-node (expression->byte-vector query))
        (root-node (((eval root-code) (sync-cons (expression->byte-vector root-code) (sync-null))))))
    (set! *sync-state* (sync-cons control-node (sync-cons (sync-cons step-node query-node)
                                                          (sync-cons secret-node root-node)))))

  "Installed control module")
