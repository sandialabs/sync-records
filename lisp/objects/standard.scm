;; Current shortcomings
;; - Signature/docstring doesn't propagate to api level
;; - Methods don't override
;; - no handling of init method

(lambda (root)

  (define src
    '(define-class (standard)
       "This is the standard interface for functions"

       (define* (make self class (external ()) (init ()))
         (if (not (eq? (car class) 'define-class))
             (error 'make-error "Please load in a class definition"))

         (let* ((name (caadr class))
                (imports (cdr (cadr class)))
                (methods (let loop ((body (cddr class)) (methods '()))
                           (cond ((null? body) (reverse methods))
                                 ((string? (car body)) (loop (cdr body) methods))
                                 (else (if (not (or (eq? (caar body) 'define) (eq? (caar body) 'define*)))
                                           (error 'component-error
                                                  "Only 'define and 'define* expressions are allowed within define-class"))
                                       (loop (cdr body)
                                             (cons `(,(caadar body) (,(if (eq? (caar body) 'define) 'lambda 'lambda*)
                                                                     ,(cdadar body) ,@(cddar body))) methods))))))
                (api (let ((proc (lambda (x) (append " " (symbol->string (car x))))))
                       (substring (apply append (map proc methods)) 1)))
                (description (append "--- Standard Class ---\n"
                                     "Name: " (symbol->string name) "\n"
                                     "Description: " (if (string? (caddr class)) (caddr class) "") "\n"
                                     "Functions: " api "\n"
                                     "-------------------------"))
                (err '(error 'function-error "Function not recognized"))
                (common `(((*name*) ,name) ((*api*) '(*name* *api* *class* ,@(map car methods))) ((*class*) ,class)))
                (prep (lambda (x) `((,(car x)) (lambda args (apply ,(cadr x) (cons self args))))))
                (get '(lambda (path)
                        (let loop ((node (self)) (path path))
                          (if (null? path) node
                              (if (zero? (car path))
                                  (loop (sync-car node) (cdr path))
                                  (loop (sync-cdr node) (cdr path)))))))
                (set '(lambda*
                       (arg-1 arg-2)
                       (set! state
                             (let loop ((node (self)) (path (if arg-2 arg-1 '())))
                               (if (null? path) (if arg-2 arg-2 arg-1)
                                   (if (zero?  (car path))
                                       (sync-cons (loop (sync-car node) (cdr path)) (sync-cdr node))
                                       (sync-cons (sync-car node) (loop (sync-cdr node) (cdr path)))))))))
                (function `(lambda (state)
                             (define* (self arg) ,description
                               (set!(setter self) ,set)
                               (cond ((not arg) state)
                                     ((list? arg) (,get arg))
                                     (else (with-let (sublet (rootlet) 'self self '*function* arg)
                                                     (let (,@(map (lambda (x y) `(,x ,y)) imports external))
                                                       (case *function*
                                                         ,@common
                                                         ,@(map prep methods)
                                                         (else ,err)))))))))
                (object ((eval function) (sync-cons (expression->byte-vector function) (sync-null)))))
           (if (member '*init* (object '*api*))
               (apply (object '*init*) init))
           object))

       (define (dump self object)
         (object))

       (define (load self node)
         ((eval (byte-vector->expression (sync-car node))) node))

       (define (serialize self node)
         (let ((ls '())
               (tb (hash-table))
               (sym (lambda (x) (string->symbol (append "n-" x)))))
           (let recurse ((node node))
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
             (map (lambda (x) (compact (map shorten x))) ls))))


       (define (deserialize self serialization)
         (let* ((proc (lambda (x)
                        (let ((k (car x)) (v (cadr x)))
                          (case (car v)
                            ((c) `(define ,k  ,(hex-string->byte-vector (cadr v))))
                            ((s) `(define ,k  ,(sync-stub (hex-string->byte-vector (cadr v)))))
                            (else `(define ,k (sync-cons ,(car v) ,(cadr v))))))))
                (expr `(begin (define n-0 (sync-null))
                              ,@(map proc (reverse serialization)))))
           (eval expr)))))

  (define (self-make class)
    "Use standard class make function to build itself"
    (let* ((function (let loop ((body class))
                       (let ((item (car body)))
                         (if (and (pair? item) (eq? (car item) 'define*) (eq? (caadr item) 'make))
                             (cons 'define* (cdr item))
                             (loop (cdr body)))))))
      ((eval function) #f class)))

  ((root 'set!) '(control library standard) `(object ,((self-make src))))

  "Installed standard library")
