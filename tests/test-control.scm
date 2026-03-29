(lambda (control-src)
  (let* ((trunc (lambda (x y) (if (< (length x) y) x (append (substring x 0 y) " ..."))))
         (test (lambda (x)
                 (let ((expected (cadr x))
                       (result (sync-call (car x) #t))
                       (condition (cond ((null? (cdr x)) '(lambda (x) #t))
                                        ((and (pair? (cadr x)) (eq? (caadr x) 'lambda))
                                         (cadr x))
                                        (else `(lambda (result) (equal? result ,(cadr x)))))))
                   (if ((eval condition) result) #t
                       (error 'assertion-failure
                              (append "Query [" (trunc (object->string (car x)) 256)
                                      "] returned [" (trunc (object->string result) 256)
                                      "] which failed assertion [" (object->string condition)
                                      "]"))))))
         (return (lambda (x) (append "Success (" (object->string (length x)) " checks)"))))
    (return
     (map test
          `(((sync-call '(,control-src "pass" #t) #t) "Installed control module")
            (((root 'set!) '(a b) 2) #t)
            (((root 'set!) '(a c d) 4) #t)
            (((root 'set!) '(a c* d) 4) #t)
            (((root 'get) '(a c d)) 4)
            (((root 'set!) '(a e f) 9) #t)
            (((root 'set!) '(a e g) 10) #t)

            ;; delete
            (((root 'set!) '(a e f) '(nothing)) #t)
            (((root 'set!) '(a e) '(nothing)) #t)

            ;; getting
            (((root 'get) '(a)) '(directory ((c* directory) (c directory) (b value))))
            (((root 'get) '(a b)) 2)
            (((root 'get) '(a c d)) 4)
            (((root 'get) '(a c* d)) 4)

            ;; equality
            (((root 'equal?) '(a b) '(a c)) #f)
            (((root 'equal?) '(a c) '(a c*)) #t)

            ;; copying
            (((root 'copy!) '(a) '(a*)) #t)
            (((root 'get) '(a* c d)) 4)
            (((root 'set!) '(a fn) (lambda (x) x))
             (lambda (x) (and (list? x) (eq? (car x) 'error))))
            (((root 'set!) '(a mac) (macro (x) x))
             (lambda (x) (and (list? x) (eq? (car x) 'error))))

            ;; objects
            (((root 'set!) '(b) (sync-cons #u(0) #u(1))) #t)
            (((root 'set!) '(b*) (sync-cut (sync-cons #u(0) #u(1)))) #t)

            ;; equivalency
            (((root 'equal?) '(b) '(b*)) #f)
            (((root 'equivalent?) '(b) '(b*)) #t))))))
