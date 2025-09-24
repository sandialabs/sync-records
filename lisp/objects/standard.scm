;; Current shortcomings
;; - Signature/docstring doesn't propagate to api level
;; - Private api doesn't show up
;; - Methods don't override

(lambda (root)

  (define standard-library
    '(define-class (standard)
       "This is the standard interface for functions"

       (define (make self code . external)
         (if (not (eq? (car code) 'define-class))
             (error 'make-error "Please load in a class definition"))

         (let* ((name (caadr code))
                (imports (cdr (cadr code)))
                (methods (let loop ((body (cddr code)) (public '()) (private '()))
                         (cond ((null? body) `(,(reverse public) ,(reverse private)))
                               ((string? (car body)) (loop (cdr body) public private))
                               (else (let ((parse (lambda (ls) 
                                                    (cons `(,(caadar body) (,(if (eq? (caar body) 'define) 'lambda 'lambda*)
                                                                            ,(cdadar body) ,@(cddar body))) ls))))
                                       (if (or (eq? (caar body) 'define) (eq? (caar body) 'define*))
                                           (if (eq? ((symbol->string (caadar body)) 0) #\~)
                                               (loop (cdr body) public (parse private))
                                               (loop (cdr body) (parse public) private))
                                           (error 'component-error
                                                  "Only 'define and 'define* expressions are allowed within define-class")))))))
                (api (let ((proc (lambda (x) (append " " (symbol->string (car x))))))
                       (substring (apply append (map proc (car methods))) 1)))
                (description (append "--- Standard Class ---\n"
                                     "Name: " (symbol->string name) "\n"
                                     "Description: " (if (string? (caddr code)) (caddr code) "") "\n"
                                     "Functions: " api "\n"
                                     "-------------------------"))
                (err '(error 'function-error "Function not recognized"))
                (common `(((*name*) ,name) ((*api*) ,(append "*name* *api* *source* " api)) ((*source*) ,code)))
                (prep (lambda (x) `((,(car x)) (lambda args (apply ,(cadr x) (cons self args))))))
                (core `(define (,name function)
                         (with-let (sublet (rootlet) 'self self ',name ,name '*function* function)
                                   (let (,@(map (lambda (x y) `(,x ,y)) imports external))
                                     (case *function*
                                       ,@common
                                       ,@(map prep (car methods))
                                       ,@(map prep (cadr methods))
                                       (else ,err))))))
                (final `(lambda (state)
                          (define* (self function) ,description
                            (set! (setter self) (lambda (new) (set! state new) #t))
                            (if (not function) state
                                (case function
                                  ((,@(map caar common) ,@(map car (car methods))) (,core function))
                                  (else ,err)))))))
           ((eval final) (sync-cons (expression->byte-vector final) (sync-null)))))

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
                         (if (and (pair? item) (eq? (car item) 'define) (eq? (caadr item) 'make))
                             (cons 'define (cdr item))
                             (loop (cdr body)))))))
      ((eval function) #f class)))

  ((root 'set!) '(control library standard) `(object ,((self-make standard-library))))

  "Installed standard library")
