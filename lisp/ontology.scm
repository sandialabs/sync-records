;; todos
;; - decide out how to fit ledger in somehow
;; - figure out interfacing

;; how to attach to standard?
;; - not sure I need to

;; how to attach to ledger?
;; - by path? --> not really possible I guess
;; - pass into each call?
;; - store your own ledger?
;; - need to handle weird root somehow

;; no...
;; - ontology is a thing that you can write to the ledger
;; - (get (s p o))
;; - (set (s p o) #t)
;; - (slice (s p o)) -> error
;; - (prune (s p o)) -> error
;; - 

;; how to handle triples set?
;; - should definitely be an object

(macro (path)

  (define src

    '(define-class (ontology)

       (define (*init* self ledger-path root-class?)
         '())

       (define (~hash self s p o)
         (sync-hash (apply append (map expression->byte-vector `(,s ,p ,o)))))

       (define (~store self s p o)
         (string->symbol (append (if (equal? s '(var)) "-" "s")
                                 (if (equal? p '(var)) "-" "p")
                                 (if (equal? o '(var)) "-" "o"))))

       (define (~n-triples self triples)
         (let* ((join (lambda (ls c)
                        (let ((str (apply append (map (lambda (x) (append x c)) ls))))
                          (substring str 0 (- (length str) 1)))))
                (str (lambda (x)
                       (case (car x)
                         ((ref) (append "<urn:sync-web:"
                                        (join (map symbol->string (cadr x)) ":")
                                        ">"))
                         ((exp) (append (object->string (cadr x))))
                         ((bno) (append "_:b" (byte-vector->hex-string (cadr x))))
                         (else (error 'type-error "Unrecognized RDF type")))))) 
           (let loop ((triples triples) (output '()))
             (if (null? triples) (join (reverse output) "\n")
                 (loop (cdr triples)
                       (cons (append (join (map str (car triples)) " ") " .")
                             output))))))

       (define (~triples-all self path index)
         (let ((ledger ((self '~ledger)))
               (record-init (eval (cadr ((record 'get) '(record library record))))))
           (let ((result ((ledger 'get) path index)))
             (if (equal? result '(nothing)) result
                 ((record 'set!) '(control scratch triples) (cadr result))))
           (let ((root-get (lambda ()
                             (let ((result ((record 'get) '(control scratch triples))))
                               (if (eq? (car result) 'nothing) (sync-null)
                                   (sync-cdr (cadr result))))))
                 (root-set! (lambda () (error 'set-error "Read-only record") #f)))
             (let ((subrecord (record-init root-get root-set!)))
               (let loop ((in (cadr ((subrecord 'get) '()))) (out '()))
                 (if (null? in)
                     (if (null? out) '(nothing ()) `(object ,out))
                     (let ((triple ((subrecord 'get) `(,(car in)))))
                       (loop (cdr in) (cons (cadr triple) out)))))))))

       (define (~triples-set! self path s p o value)
         (let ((ledger ((self '~ledger))))
           (let ((root ((ledger 'get) path))
                 (type (expression->byte-vector 'ontology-triples)))
             (if (or (not (eq? (car root) 'structure))
                     (not (byte-vector? (sync-car (cadr root))))
                     (not (eq? (byte-vector->expression (sync-car (cadr root))) 'ontology-triples)))
                 ((ledger 'set!) path (sync-cons type (sync-null))))
             (let ((record-init (eval (cadr ((record 'get) '(record library record)))))
                   (root-get (lambda ()
                               (let ((result ((ledger 'get) path)))
                                 (if (equal? result '(nothing)) (sync-null)
                                     (sync-cdr (cadr result))))))
                   (root-set! (lambda (value)
                                ((ledger 'set!) path (sync-cons type value)))))
               (let ((subrecord (record-init root-get root-set!)))
                 ((subrecord 'set!) `(,((self '~hash) s p o)) value)
                 )))))

       (define (~check self s p o)
         (cond ((not (and (pair? s) (pair? p) (pair? o)))
                (error 'type-error "Ontology terms must be pairs"))
               ((not (or (eq? (car s) 'ref) (eq? (car s) 'var)))
                (error 'type-error "Subject must be a reference or variable"))
               ((not (or (eq? (car p) 'ref) (eq? (car p) 'var)))
                (error 'type-error "Predicate must be a reference or variable"))
               ((not (or (eq? (car o) 'ref) (eq? (car o) 'var) (eq? (car o) 'exp)))
                (error 'type-error "Object must be a reference, variable, or expression"))
               (else #t)))

       (define (~assign self x)
         (if (and (eq? (car x) 'var) (null? (cdr x)))
             `(var ,(random-byte-vector 32))
             x))

       (define (get self triple)
         "Select triples matching (s, p, o) from the ontology graph.

      > record (fnc): library to access record commands
      > s (exp): subject
      > p (exp): predicate
      > o (exp): object
      > graph (list|#f): graph path or #f to refer to the local graph
      > index (int): ledger index
      < return (list triples): list of matching triples"

         ((self '~check) s p o)
         (let ((graph (if graph graph '(*state* *ontology*)))
               (store ((self '~store) s p o))
               (store-id ((self '~hash) s p o)))
           (let ((path (append graph `(,store ,store-id))))
             ((self '~triples-all) record path index))))

       (define (set! triple value)
         (if value
             (apply (self '~insert) triple)
             (apply (self '~remove) triple)))

       (define (prune! triple)
         (error 'implementation-error "Prune operation not implemented for ontology"))

       (define (slice! triple)
         (error 'implementation-error "Slice operation not implemented for ontology"))

       (define (~operate self s p o graph value)
         ((self '~check) s p o)
         (let loop ((ls `((,s ,p ,o)
                          (,s ,p (var)) (,s (var) ,o) ((var) ,p ,o)
                          (,s (var) (var)) ((var) ,p (var)) ((var) (var) ,o)
                          ((var) (var) (var)))))
           (if (null? ls) #t
               (let ((store (apply (self '~store) (car ls)))
                     (store-id (apply (self '~hash) (car ls))))
                 (let ((path (append graph `(,store ,store-id))))
                   ((self '~triples-set!) record path s p o value)
                   (loop (cdr ls)))))))

       (define (~insert! self s p o)
         "Insert a triple (s, p, o) into the ontology graph.

      > record (fnc): library to access record commands
      > s (exp): subject
      > p (exp): predicate
      > o (exp): object
      > graph (list|#f): graph path or default
      < return (bool): success"
         (let ((graph (if graph graph '(*state* *ontology*))))
           (let ((s ((self '~assign) s))
                 (p ((self '~assign) p))
                 (o ((self '~assign) o)))
             ((self '~operate) record s p o graph `(,s ,p ,o)))))

       (define (~remove! self s p o)
         "Remove a triple (s, p, o) from the ontology graph.

      > record (fnc): library to access record commands
      > s (exp): subject
      > p (exp): predicate
      > o (exp): object
      > graph (list|#f): graph path or default
      < return (bool): success"
         (let ((graph (if graph graph '(*state* *ontology*))))
           ((self '~operate) record s p o graph #f)))

      ;;  (define* (insert-batch! self triples graph)
      ;;    "Insert a batch of triples into the ontology graph.

      ;; > record (fnc): library to access record commands
      ;; > triples (list): list of triples
      ;; > graph (list|#f): graph path or default
      ;; < return (bool): success"
      ;;    (let ((graph (if graph graph '(*state* *ontology*))))
      ;;      (let loop ((triples triples))
      ;;        (if (null? triples) #t
      ;;            (let ((s (caar triples)) (p (cadar triples)) (o (caddar triples)))
      ;;              (let ((s ((self '~assign) s))
      ;;                    (p ((self '~assign) p))
      ;;                    (o ((self '~assign) o)))
      ;;                ((self '~operate) record s p o graph `(,s ,p ,o)))
      ;;              (loop (cdr triples)))))))

      ;;  (define* (remove-batch! self triples graph)
      ;;    "Remove a batch of triples from the ontology graph.

      ;; > record (fnc): library to access record commands
      ;; > triples (list): list of triples
      ;; > graph (list|#f): graph path or default
      ;; < return (bool): success"
      ;;    (let ((graph (if graph graph '(*state* *ontology*))))
      ;;      (let loop ((triples triples))
      ;;        (if (null? triples) #t
      ;;            (let ((s (caar triples)) (p (cadar triples)) (o (caddar triples)))
      ;;              ((self '~operate) record s p o graph #f)
      ;;              (loop (cdr triples)))))))

      ;;  (define (dfs self term depth n-triples?)
      ;;    "Depth-first search over the ontology graph, optionally formatting as an
      ;; N-triples document.

      ;; > record (fnc): library to access record commands
      ;; > term (exp): starting term
      ;; > depth (int): recursion depth
      ;; > n-triples? (bool): output as N-Triples if #t
      ;; < return (list|string): result triples or N-Triples string"
      ;;    (let ((seen (hash-table)))
      ;;      (let ((output
      ;;             (let recurse ((term term) (depth depth))
      ;;               (cond ((= depth 0) '())
      ;;                     ((not (eq? (car term) 'ref)) '())
      ;;                     ((eq? (caadr term) '~) '())
      ;;                     ((seen term) '())
      ;;                     (else
      ;;                      (let* ((parts (let loop ((head '()) (tail (cadr term)))
      ;;                                      (if (eq? (car tail) '*state*)
      ;;                                          `(,(reverse head) ,tail)
      ;;                                          (loop (cons (car tail) head) (cdr tail)))))
      ;;                             (result ((self 'select) record `(ref ,(cadr parts)) '(var) '(var)
      ;;                                      (append (car parts) '(*state* *ontology*))))
      ;;                             (prepend (lambda (t)
      ;;                                        (map (lambda (x)
      ;;                                               (if (or (not (eq? (car x) 'ref))
      ;;                                                       (eq? (caadr x) '~)) x
      ;;                                                       `(ref ,(append (car parts) (cadr x))))) t))))
      ;;                        (if (equal? result '(nothing)) ()
      ;;                            (let loop ((in (map prepend (cadr result))) (out '()))
      ;;                              (if (null? in) (begin (set! (seen term) #t) out)
      ;;                                  (loop (cdr in) (append (list (car in))
      ;;                                                         (recurse (cadar in) (- depth 1))
      ;;                                                         (recurse (caddar in) (- depth 1))
      ;;                                                         out)))))))))))
      ;;        (if n-triples? ((self '~n-triples) output) output))))
       ))
  
  `(lambda (root)
     ((root 'set!) ,path ',src)))
