(lambda (run-test make-messenger control-src standard-src record-src)
  (let* ((pass (lambda (x) (append "pass-" (symbol->string x))))
         (init (lambda (x) `(,x (,control-src ,(pass x)) "Installed control module")))
         (install (lambda (x) `(,(car x) (*call* ,(pass (car x)) ,(cadr x)) #t)))
         (instantiate (lambda (args)
                        (let ((journal-name (car args))
                              (obj-name (cadr args))
                              (cls-path (caddr args))
                              (make-args (cdddr args)))
                          `(,journal-name
                            (*call* ,(pass journal-name)
                                    (lambda (root)
                                      (let* ((std-node (cadr ((root 'get) '(control library standard))))
                                             (std ((eval (byte-vector->expression (sync-car std-node))) std-node))
                                             (cls (cadr ((root 'get) ',cls-path)))
                                             (obj (apply (std 'make) (cons cls ,make-args))))
                                        ((root 'set!) '(control test ,obj-name) `(content ,(obj)))))) #t))))
         (query (lambda (args)
                  `(,(car args)
                    (*call* ,(pass (car args))
                            (lambda (root)
                              (let* ((std-node (cadr ((root 'get) '(control library standard))))
                                     (std ((eval (byte-vector->expression (sync-car std-node))) std-node))
                                     ,@(map (lambda (x)
                                              `(,x ((std 'load) (cadr ((root 'get) '(control test ,x))))))
                                            (cadr args))
                                     (*result* ,(caddr args)))
                                ,@(map (lambda (x)
                                         `((root 'set!) '(control test ,x) `(content ,(,x))))
                                       (cadr args))
                                *result*)))
                    ,@(cdddr args)))))
    (run-test
     (append
      (map init '(journal))
      (map install `((journal ,standard-src "Installed standard library")
                     (journal ,record-src "Installed record class")))
      (map instantiate `((journal record-1 (control library record))
                         (journal record-2 (control library record))))
      (map query
           `((journal (record-1) ((record-1 'set!) '(a b) 2) #t)
             (journal (record-1) ((record-1 'set!) '(a c d) 4) #t)
             (journal (record-1) ((record-1 'set!) '(a c* d) 4) #t)
             (journal (record-1) ((record-1 'get) '(a c d)) '(content 4))
             (journal (record-1) ((record-1 'set!) '(a e f) 9) #t)
             (journal (record-1) ((record-1 'set!) '(a e g) 10) #t)

             ;; delete
             (journal (record-1) ((record-1 'set!) '(a e f) #f) #t)
             (journal (record-1) ((record-1 'set!) '(a e) #f) #t)

             ;; getting
             (journal (record-1) ((record-1 'get) '(a)) '(directory (c c* b) #t))
             (journal (record-1) ((record-1 'get) '(a b)) '(content 2))
             (journal (record-1) ((record-1 'get) '(a c d)) '(content 4))
             (journal (record-1) ((record-1 'get) '(a c* d)) '(content 4))

             ;; equality
             (journal (record-1) ((record-1 'equal?) '(a b) '(a c)) #f)
             (journal (record-1) ((record-1 'equal?) '(a c) '(a c*)) #t)

             ;; copying
             (journal (record-1) ((record-1 'copy!) '(a) '(a*)) #t)
             (journal (record-1) ((record-1 'get) '(a* c d)) '(content 4))

             ;; slicing
             (journal (record-1) ((record-1 'get) '(a)) '(directory (c c* b) #t))
             (journal (record-1) ((record-1 'slice!) '(a) '(b)) #t)
             (journal (record-1) ((record-1 'get) '(a b)) '(content 2))
             (journal (record-1) ((record-1 'get) '(a)) '(directory (b) #f))

             ;; serialization
             (journal (record-1) ((record-1 'deserialize!) '(a*) ((record-1 'serialize) '(a))) #t)
             (journal (record-1) ((record-1 'equal?) '(a* b) '(a b)) #t)
             (journal (record-1) ((record-1 'deserialize!) '(a*) ((record-1 'serialize) '(a))) #t)
             (journal (record-1) ((record-1 'equal?) '(a*) '(a)) #t)

             ;; pruning
             (journal (record-1) ((record-1 'set!) '(b a c d) 4) #t)
             (journal (record-1) ((record-1 'set!) '(b d d) 2) #t)
             (journal (record-1) ((record-1 'set!) '(b d e) 5) #t)
             (journal (record-1) ((record-1 'set!) '(b n c d) 1) #t)
             (journal (record-1) ((record-1 'copy!) '(b) '(b*)) #t)
             (journal (record-1) ((record-1 'prune!) '(b) '(d d) #t) #t)
             (journal (record-1) ((record-1 'prune!) '(b) '(d e)) #t)
             (journal (record-1) ((record-1 'get) '(b d)) '(directory (d) #f))
             (journal (record-1) ((record-1 'get) '(b d d)) '(unknown))
             (journal (record-1) ((record-1 'get) '(b n c d)) '(content 1))

             ;; equivalency
             (journal (record-1) ((record-1 'equal?) '(b) '(b*)) #f)
             (journal (record-1) ((record-1 'equivalent?) '(b) '(b*)) #t)

             ;; merging
             (journal (record-1) ((record-1 'merge!) '(b) '(b*)) #t)
             (journal (record-1) ((record-1 'equivalent?) '(b) '(b*)) #t)
             (journal (record-1) ((record-1 'get) '(b* d d)) '(content 2))
             ))
      ))))
