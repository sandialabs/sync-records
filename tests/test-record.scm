(lambda (run-test make-messenger record-src control-src)
  (let* ((pass (lambda (x) (append "pass-" (symbol->string x))))
         (init (lambda (x) `(,x (,record-src ,(pass x) ,control-src) "Installed record interface")))
         (form (lambda (x)
                 `(,(car x) (*record* ,(pass (car x)) (lambda (record) ,(cadr x))) ,@(cddr x)))))
    (run-test
     (append
      (map init '(journal))
      (map form
           `((journal ((record 'set!) '(a b) 2) #t)
             (journal ((record 'set!) '(a c d) 4) #t)
             (journal ((record 'set!) '(a c* d) 4) #t)
             (journal ((record 'get) '(a c d)) '(object 4))
             (journal ((record 'set!) '(a e f) 9) #t)
             (journal ((record 'set!) '(a e g) 10) #t)

             ;; delete
             (journal ((record 'set!) '(a e f) #f) #t)
             (journal ((record 'set!) '(a e) #f) #t)

             ;; getting
             (journal ((record 'get) '(a)) '(directory (c c* b) #t))
             (journal ((record 'get) '(a b)) '(object 2))
             (journal ((record 'get) '(a c d)) '(object 4))
             (journal ((record 'get) '(a c* d)) '(object 4))

             ;; equality
             (journal ((record 'equal?) '(a b) '(a c)) #f)
             (journal ((record 'equal?) '(a c) '(a c*)) #t)

             ;; copying
             (journal ((record 'copy!) '(a) '(a*)) #t)
             (journal ((record 'get) '(a* c d)) '(object 4))

             ;; slicing
             (journal ((record 'get) '(a)) '(directory (c c* b) #t))
             (journal ((record 'slice!) '(a) '(b)) #t)
             (journal ((record 'get) '(a b)) '(object 2))
             (journal ((record 'get) '(a)) '(directory (b) #f))

             ;; serialization
             (journal ((record 'deserialize!) '(a*) ((record 'serialize) '(a))) #t)
             (journal ((record 'equal?) '(a* b) '(a b)) #t)
             (journal ((record 'deserialize!) '(a*) ((record 'serialize) '(a))) #t)
             (journal ((record 'equal?) '(a*) '(a)) #t)

             ;; pruning
             (journal ((record 'set!) '(b a c d) 4) #t)
             (journal ((record 'set!) '(b d d) 2) #t)
             (journal ((record 'set!) '(b d e) 5) #t)
             (journal ((record 'set!) '(b n c d) 1) #t)
             (journal ((record 'copy!) '(b) '(b*)) #t)
             (journal ((record 'prune!) '(b) '(d d) #t) #t)
             (journal ((record 'prune!) '(b) '(d e)) #t)
             (journal ((record 'get) '(b d)) '(directory (d) #f))
             (journal ((record 'get) '(b d d)) '(unknown ()))
             (journal ((record 'get) '(b n c d)) '(object 1))

             ;; equivalency
             (journal ((record 'equal?) '(b) '(b*)) #f)
             (journal ((record 'equivalent?) '(b) '(b*)) #t)

             ;; merging
             (journal ((record 'merge!) '(b) '(b*)) #t)
             (journal ((record 'equivalent?) '(b) '(b*)) #t)
             (journal ((record 'get) '(b* d d)) '(object 2))))))))
