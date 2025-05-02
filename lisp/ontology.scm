(lambda (record secret)

  (define ontology-hash
    '(lambda (s p o)
       (sync-hash (apply append (map expression->byte-vector `(,s ,p ,o))))))

  (define ontology-store
    '(lambda (s p o)
       (string->symbol (append (if (equal? s '(var)) "-" "s")
                               (if (equal? p '(var)) "-" "p")
                               (if (equal? o '(var)) "-" "o")))))

  (define triples-load
    '(lambda (record path)
       (let ((root ((record 'get) path))
             (type (expression->byte-vector 'ontology-triples)))
         (if (or (not (sync-pair? root))
                 (not (byte-vector? (sync-car root)))
                 (eq? (byte-vector->expression (sync-car root)) 'triples-set))
             ((record 'set!) path (sync-cons type (sync-null))))
         (let ((record-new (eval (cadr ((record 'get) '(record library record)))))
               (root-get (lambda () (sync-cdr ((record 'get) path))))
               (root-set! (lambda (value)
                            ((record 'set!) path (sync-cons type (if value value (sync-null)))))))
           (record-new root-get root-set!)))))

  (define triples-all
    '(lambda (triples)
       (let loop ((in (cadr ((triples 'get) '()))) (out '()))
         (if (null? in) out
             (let ((triple ((triples 'get) `(,(car in)))))
               (loop (cdr in) (cons triple out)))))))

  (define triples-set!
    `(lambda (triples s p o value)
       ((triples 'set!) `(,(,ontology-hash s p o)) value)))

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
      (let ((ledger-get ((eval (cadr ((record 'get) '(record library ledger)))) 'get))
            (store (,ontology-store s p o))
            (store-id (,ontology-hash s p o)))
        (let ((path `(*state* *ontology* ,store ,store-id)))
          (,triples-all (,triples-load record path))))))

  (define ontology-operate
    `(lambda (record s p o value)
       (,ontology-check s p o)
       (let ((ledger-set! ((eval (cadr ((record 'get) '(record library ledger)))) 'set!)))
         (let loop ((ls `((,s ,p ,o)
                          (,s ,p (var)) (,s (var) ,o) ((var) ,p ,o)
                          (,s (var) (var)) ((var) ,p (var)) ((var) (var) ,o)
                          ((var) (var) (var)))))
           (if (null? ls) #t
               (let ((store (apply ,ontology-store (car ls)))
                     (store-id (apply ,ontology-hash (car ls))))
                 (let ((path `(*state* *ontology* ,store ,store-id)))
                   (,triples-set! (,triples-load record path) s p o value)
                   (loop (cdr ls)))))))))

  (define ontology-insert!
    `(lambda (record s p o)
       (let ((ontology-assign ,ontology-assign))
         (let ((s (ontology-assign s))
               (p (ontology-assign p))
               (o (ontology-assign o)))
           (,ontology-operate record s p o `(,s ,p ,o))))))

  (define ontology-remove!
    `(lambda (record s p o)
       (,ontology-operate record s p o #f)))

  (define ontology-library
    `(lambda (function)
       (case function
         ((select) ,ontology-select)
         ((insert!) ,ontology-insert!)
         ((remove!) ,ontology-remove!)
         (else (error 'missing-function "Function not found")))))
  
  ((record 'set!) '(control local ontology-select) ontology-select)
  ((record 'set!) '(control local ontology-insert!) ontology-insert!)
  ((record 'set!) '(control local ontology-remove!) ontology-remove!)
  ((record 'set!) '(record library ontology) ontology-library))
