(lambda (run-test make-messenger control-src standard-src tree-src)
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
                                      (let* ((std-node ((root 'get) '(control object standard)))
                                             (std ((eval (byte-vector->expression (sync-car std-node))) std-node))
                                             (cls ((root 'get) ',cls-path))
                                             (obj (apply (std 'make) (cons cls ',make-args))))
                                        ((root 'set!) '(control test ,obj-name) (obj))))) #t))))
         (query (lambda (args)
                  `(,(car args)
                    (*call* ,(pass (car args))
                            (lambda (root)
                              (let* ((std-node ((root 'get) '(control object standard)))
                                     (std ((eval (byte-vector->expression (sync-car std-node))) std-node))
                                     ,@(map (lambda (x)
                                              `(,x ((std 'load) ((root 'get) '(control test ,x)))))
                                            (cadr args))
                                     (*result* ,(caddr args)))
                                ,@(map (lambda (x)
                                         `((root 'set!) '(control test ,x) (,x)))
                                       (cadr args))
                                *result*)))
                    ,@(cdddr args)))))
    (run-test
     (append
      (map init '(journal))
      (map install `((journal (,standard-src '(control class standard) '(control object standard)) "Installed standard library")
                     (journal (lambda (root) ((root 'set!) '(control class tree) ',tree-src)))))
      (map instantiate `((journal tree-1 (control class tree))
                         (journal tree-2 (control class tree))
                         (journal tree-3 (control class tree))))
      (map query
           `((journal (tree-1) ((tree-1 'set!) '(a b) 2) #t)
             (journal (tree-1) ((tree-1 'set!) '(a c d) 4) #t)
             (journal (tree-1) ((tree-1 'set!) '(a c* d) 4) #t)
             (journal (tree-1) ((tree-1 'get) '(a c d)) 4)
             (journal (tree-1) ((tree-1 'set!) '(a e f) 9) #t)
             (journal (tree-1) ((tree-1 'set!) '(a e g) 10) #t)

             ;; delete
             (journal (tree-1) ((tree-1 'set!) '(a e f) '(nothing)) #t)
             (journal (tree-1) ((tree-1 'set!) '(a e) '(nothing)) #t)

             ;; getting
             (journal (tree-1) ((tree-1 'get) '(a)) '(directory (c c* b) #t))
             (journal (tree-1) ((tree-1 'get) '(a b)) 2)
             (journal (tree-1) ((tree-1 'get) '(a c d)) 4)
             (journal (tree-1) ((tree-1 'get) '(a c* d)) 4)

             ;; equality
             (journal (tree-1) ((tree-1 'equal?) '(a b) '(a c)) #f)
             (journal (tree-1) ((tree-1 'equal?) '(a c) '(a c*)) #t)

             ;; copying
             (journal (tree-1) ((tree-1 'copy!) '(a) '(a*)) #t)
             (journal (tree-1) ((tree-1 'get) '(a* c d)) 4)

             ;; slicing
             (journal (tree-1) ((tree-1 'get) '(a)) '(directory (c c* b) #t))
             (journal (tree-1) ((tree-1 'slice!) '(a b)) #t)
             (journal (tree-1) ((tree-1 'get) '(a b)) 2)
             (journal (tree-1) ((tree-1 'get) '(a)) '(directory (b) #f))

             ;; pruning
             (journal (tree-1) ((tree-1 'set!) '(b a c d) 4) #t)
             (journal (tree-1) ((tree-1 'set!) '(b d d) 2) #t)
             (journal (tree-1) ((tree-1 'set!) '(b d e) 5) #t)
             (journal (tree-1) ((tree-1 'set!) '(b n c d) 1) #t)
             (journal (tree-1) ((tree-1 'copy!) '(b) '(b*)) #t)
             (journal (tree-1) ((tree-1 'prune!) '(b d d) #t) #t)
             (journal (tree-1) ((tree-1 'prune!) '(b d e)) #t)
             (journal (tree-1) ((tree-1 'get) '(b d)) '(directory (d) #f))
             (journal (tree-1) ((tree-1 'get) '(b d d)) '(unknown))
             (journal (tree-1) ((tree-1 'get) '(b n c d)) 1)

             ;; equivalency
             (journal (tree-1) ((tree-1 'equal?) '(b) '(b*)) #f)
             (journal (tree-1) ((tree-1 'equivalent?) '(b) '(b*)) #t)

             ;; validity
             (journal (tree-1) ((tree-1 'valid?) #t))

             ;; merging
             (journal (tree-2) ((tree-2 'set!) '(a b) 2) #t)
             (journal (tree-2) ((tree-2 'set!) '(a* b) 4) #t)
             (journal (tree-2) ((tree-2 'prune!) '(a b)) #t)
             (journal (tree-3) ((tree-3 'set!) '(a b) 2) #t)
             (journal (tree-3) ((tree-3 'set!) '(a* b) 4) #t)
             (journal (tree-3) ((tree-3 'prune!) '(a* b)) #t)
             (journal (tree-2 tree-3) ((tree-2 'merge!) tree-3) #t)
             (journal (tree-2) ((tree-2 'get) '(a b)) 2)
             (journal (tree-2) ((tree-2 'get) '(a* b)) 4)
             ))))))
