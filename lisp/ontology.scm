(lambda (record secret)

  (define ontology-hash
    '(lambda (s p o)
       (sync-hash (apply append (map expression->byte-vector `(,s ,p ,o))))))

  (define ontology-store
    '(lambda (s p o)
       (string->symbol (append (if (equal? s '(var)) "-" "s")
                               (if (equal? p '(var)) "-" "p")
                               (if (equal? o '(var)) "-" "o")))))

  (define ontology-triples-all
    '(lambda (record path index)
       (let ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record))
             (record-init (eval (cadr ((record 'get) '(record library record))))))
         (let ((result ((ledger 'get) path index)))
           (if (eq? (car result) 'nothing) result
               ((record 'set!) '(control scratch triples) (cadr result))))
         (let ((root-get (lambda ()
                           (let ((result ((record 'get) '(control scratch triples))))
                             (if (eq? (car result) 'nothing) #f
                                 (sync-cdr (cadr result))))))
               (root-set! (lambda () (error 'set-error "Read-only record"))))
           (let ((subrecord (record-init root-get root-set!)))
             (let loop ((in (cadr ((subrecord 'get) '()))) (out '()))
               (if (null? in)
                   (if (null? out) '(nothing ()) `(object ,out))
                   (let ((triple ((subrecord 'get) `(,(car in)))))
                     (loop (cdr in) (cons (cadr triple) out))))))))))

  (define ontology-triples-set!
    `(lambda (record path s p o value)
       (let ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record)))
         (let ((root ((ledger 'get) path))
               (type (expression->byte-vector 'ontology-triples)))
           (if (or (not (eq? (car root) 'structure))
                   (not (byte-vector? (sync-car (cadr root))))
                   (not (eq? (byte-vector->expression (sync-car (cadr root))) 'ontology-triples)))
               ((ledger 'set!) path (sync-cons type (sync-null))))
           (let ((record-init (eval (cadr ((record 'get) '(record library record)))))
                 (root-get (lambda ()
                             (let ((result ((ledger 'get) path)))
                               (if (eq? (car result) 'nothing) #f
                                   (sync-cdr (cadr result))))))
                 (root-set! (lambda (value)
                              ((ledger 'set!) path (sync-cons type (if value value (sync-null)))))))
             (let ((subrecord (record-init root-get root-set!)))
               ((subrecord 'set!) `(,(,ontology-hash s p o)) value)
               ))))))

  (define ontology-check
    '(lambda (s p o)
       (cond ((not (and (pair? s) (pair? p) (pair? o)))
              (error 'type-error "Ontology terms must be pairs"))
             ((not (or (eq? (car s) 'ref) (eq? (car s) 'var)))
              (error 'type-error "Subject must be a reference or variable"))
             ((not (or (eq? (car p) 'ref) (eq? (car p) 'var)))
              (error 'type-error "Predicate must be a reference or variable"))
             ((not (or (eq? (car o) 'ref) (eq? (car o) 'var) (eq? (car o) 'exp)))
              (error 'type-error "Object must be a reference, variable, or expression"))
             (else #t))))

  (define ontology-assign
    '(lambda (x)
       (if (and (eq? (car x) 'var) (null? (cdr x)))
           `(var ,(random-byte-vector 32))
           x)))

  (define ontology-select
    `(lambda*
      (record s p o graph index)
      (,ontology-check s p o)
      (let ((graph (if graph graph '(*state* *ontology*)))
            (store (,ontology-store s p o))
            (store-id (,ontology-hash s p o)))
        (let ((path (append graph `(,store ,store-id))))
          (,ontology-triples-all record path index)))))

  (define ontology-operate
    `(lambda (record s p o graph value)
       (,ontology-check s p o)
       (let loop ((ls `((,s ,p ,o)
                        (,s ,p (var)) (,s (var) ,o) ((var) ,p ,o)
                        (,s (var) (var)) ((var) ,p (var)) ((var) (var) ,o)
                        ((var) (var) (var)))))
         (if (null? ls) #t
             (let ((store (apply ,ontology-store (car ls)))
                   (store-id (apply ,ontology-hash (car ls))))
               (let ((path (append graph `(,store ,store-id))))
                 (,ontology-triples-set! record path s p o value)
                 (loop (cdr ls))))))))

  (define ontology-insert!
    `(lambda*
      (record s p o graph)
       (let ((graph (if graph graph '(*state* *ontology*)))
             (ontology-assign ,ontology-assign))
         (let ((s (ontology-assign s))
               (p (ontology-assign p))
               (o (ontology-assign o)))
           (,ontology-operate record s p o graph `(,s ,p ,o))))))

  (define ontology-remove!
    `(lambda*
      (record s p o graph)
       (let ((graph (if graph graph '(*state* *ontology*))))
         (,ontology-operate record s p o graph #f))))

  (define ontology-library
    `(lambda (record)
       (lambda (function)
         (case function
           ((select) (lambda args (apply ,ontology-select (cons record args))))
           ((insert!) (lambda args (apply ,ontology-insert! (cons record args))))
           ((remove!) (lambda args (apply ,ontology-remove! (cons record args))))
           (else (error 'missing-function "Function not found"))))))
  
  ((record 'set!) '(control local ontology-select) ontology-select)
  ((record 'set!) '(control local ontology-insert!) ontology-insert!)
  ((record 'set!) '(control local ontology-remove!) ontology-remove!)
  ((record 'set!) '(record library ontology) ontology-library))
