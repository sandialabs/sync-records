;; todo
;; - have the final directory be an atomic ontology-specific object for quicker retrieval
;;   - maybe as a hashmap?

(lambda (record secret)

  (define ontology-hash
    '(lambda (s p o)
       (sync-hash (append (expression->byte-vector s)
                          (expression->byte-vector p)
                          (expression->byte-vector o)))))

  (define ontology-store
    '(lambda (s p o)
       (string->symbol (append (if s "s" "-") (if p "p" "-") (if o "o" "-")))))

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
      (let ((ledger-get ((eval (cadr ((record 'get) '(control library ledger)))) 'get))
            (store (,ontology-store s p o))
            (store-id (,ontology-hash s p o)))
        (let ((path `(*state* *ontology* ,store ,store-id)))
          (let loop ((in (cadr (ledger-get record path))) (out '()))
            (if (null? in) out
                (let ((triple (cadr (ledger-get record (append path `(,(car in)))))))
                  (loop (cdr in) (cons triple out)))))))))

  (define ontology-operate
    `(lambda (record s p o value)
       (,ontology-check s p o)
       (let ((ledger-set! ((eval (cadr ((record 'get) '(control library ledger)))) 'set!))
             (id (,ontology-hash s p o)))
         (let loop ((ls `((,s ,p ,o)
                          (,s ,p (var)) (,s (var) ,o) ((var) ,p ,o)
                          (,s (var) (var)) ((var) ,p (var)) ((var) (var) ,o)
                          ((var) (var) (var)))))
           (if (null? ls) #t
               (let ((store (apply ,ontology-store (car ls)))
                     (store-id (apply ,ontology-hash (car ls))))
                 (ledger-set! record `(*state* *ontology* ,store ,store-id ,id) value)
                 (loop (cdr ls))))))))

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

  (define ontology-clip-set!
    '(lambda (record clip expansion)
       ((record 'set!) `(ontology clipes ,clip) expansion)))

  (define ontology-clip-get
    '(lambda (record clip)
       (cadr ((record 'get) `(ontology clipes ,clip)))))

  (define ontology-clip-all
    '(lambda (record)
       (let loop ((in (cadr ((record 'get) '(ontology clipes)))) (out '()))
         (if (null? in) out
             (let ((expansion (cadr ((record 'get) `(ontology clipes ,(car in))))))
               (loop (cdr in) (cons `(,(car in) ,expansion) out)))))))

  ((record 'set!) '(control local ontology-select) ontology-select)
  ((record 'set!) '(control local ontology-insert!) ontology-insert!)
  ((record 'set!) '(control local ontology-remove!) ontology-remove!)
  ((record 'set!) '(control local ontology-clip-set!) ontology-clip-set!)
  ((record 'set!) '(control local ontology-clip-get) ontology-clip-get)
  ((record 'set!) '(control local ontology-clip-all) ontology-clip-all))
