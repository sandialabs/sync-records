(lambda (run-test make-messenger control-src)
  (let* ((pass (lambda (x) (append "pass-" (symbol->string x))))
         (init (lambda (x) `(,x (,control-src ,(pass x) #t) "Installed control module")))
         (form (lambda (x)
                 `(,(car x) (*call* ,(pass (car x)) (lambda (root) ,(cadr x))) ,@(cddr x)))))
    (run-test
     (append
      (map init '(journal))
      (map form
           `((journal ((root 'set!) '(a b) 2) #t)
             (journal ((root 'set!) '(a c d) 4) #t)
             (journal ((root 'set!) '(a c* d) 4) #t)
             (journal ((root 'get) '(a c d)) 4)
             (journal ((root 'set!) '(a e f) 9) #t)
             (journal ((root 'set!) '(a e g) 10) #t)

             ;; delete
             (journal ((root 'set!) '(a e f) '(nothing)) #t)
             (journal ((root 'set!) '(a e) '(nothing)) #t)

             ;; getting
             (journal ((root 'get) '(a)) '(directory ((c* directory) (c directory) (b value))))
             (journal ((root 'get) '(a b)) 2)
             (journal ((root 'get) '(a c d)) 4)
             (journal ((root 'get) '(a c* d)) 4)

             ;; equality
             (journal ((root 'equal?) '(a b) '(a c)) #f)
             (journal ((root 'equal?) '(a c) '(a c*)) #t)

             ;; copying
             (journal ((root 'copy!) '(a) '(a*)) #t)
             (journal ((root 'get) '(a* c d)) 4)
             (journal ((root 'set!) '(a fn) (lambda (x) x))
                      (lambda (x) (and (list? x) (eq? (car x) 'error))))
             (journal ((root 'set!) '(a mac) (macro (x) x))
                      (lambda (x) (and (list? x) (eq? (car x) 'error))))

             ;; objects
             (journal ((root 'set!) '(b) (sync-cons #u(0) #u(1))) #t)
             (journal ((root 'set!) '(b*) (sync-cut (sync-cons #u(0) #u(1)))) #t)

             ;; equivalency
             (journal ((root 'equal?) '(b) '(b*)) #f)
             (journal ((root 'equivalent?) '(b) '(b*)) #t)))))))
