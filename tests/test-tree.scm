(lambda (standard-src tree-src)

  (define asserted 0)

  (define-macro (assert expression expected)
    `(let ((trunc (lambda (x y) (if (< (length x) y) x (append (substring x 0 y) " ...")))))
       (catch #t
              (lambda ()
                (let* ((result~ ,expression)
                       (expected~ ,expected)
                       (check~ (cond ((not expected~) (lambda (x) #t))
                                     ((procedure? expected~) expected~)
                                     (else (lambda (result) (equal? result expected~))))))
                  (if (check~ result~)
                      (begin (set! asserted (+ asserted 1)) result~)
                      (error 'assertion-failure
                             (append "[Check " (object->string asserted) " failed] "
                                     "[Expression " (object->string ',expression) "] "
                                     "[Evaluated " (trunc (object->string result~) 256) "] "
                                     "[Expected " (trunc (object->string expected~) 256) "]")))))
              (lambda args
                (error 'assertion-failure
                       (append "[Check " (object->string asserted) " errored] "
                               "[Expression " (object->string ',expression) "] "
                               "[Error " (trunc (object->string args) 256) "]"))))))

  (define standard
    (let ((init (caddr standard-src)))
      (sync-eval ((eval `(lambda* ,(cddadr init) ,@(cddr init))) standard-src) #f)))

  (define tree-1 (sync-eval (assert ((standard 'init) tree-src) sync-node?) #f))
  (define tree-2 (sync-eval (assert ((standard 'init) tree-src) sync-node?) #f))
  (define tree-3 (sync-eval (assert ((standard 'init) tree-src) sync-node?) #f))

  (assert ((tree-1 'set!) '(a b) 2) #t)
  (assert ((tree-1 'set!) '(a c d) 4) #t)
  (assert ((tree-1 'set!) '(a c* d) 4) #t)
  (assert ((tree-1 'get) '(a c d)) 4)
  (assert ((tree-1 'set!) '(a e f) 9) #t)
  (assert ((tree-1 'set!) '(a e g) 10) #t)

  (assert ((tree-1 'set!) '(a e f) '(nothing)) #t)
  (assert ((tree-1 'set!) '(a e) '(nothing)) #t)

  (assert ((tree-1 'get) '(a)) '(directory ((c directory) (c* directory) (b value)) #t))
  (assert ((tree-1 'get) '(a b)) 2)
  (assert ((tree-1 'get) '(a c d)) 4)
  (assert ((tree-1 'get) '(a c* d)) 4)

  (assert ((tree-1 'equal?) '(a b) '(a c)) #f)
  (assert ((tree-1 'equal?) '(a c) '(a c*)) #t)

  (assert ((tree-1 'copy!) '(a) '(a*)) #t)
  (assert ((tree-1 'get) '(a* c d)) 4)

  ;; (assert (catch #t
  ;;           (lambda () ((tree-1 'set!) '(a fn) (lambda (x) x)))
  ;;           (lambda args args))
  ;;         (lambda (x) (and (list? x) (eq? (car x) 'value-error))))

  ;; (assert (catch #t
  ;;           (lambda () ((tree-1 'set!) '(a mac) (macro (x) x)))
  ;;           (lambda args args))
  ;;         (lambda (x) (and (list? x) (eq? (car x) 'value-error))))

  (assert ((tree-1 'get) '(a)) '(directory ((c directory) (c* directory) (b value)) #t))
  (assert ((tree-1 'slice!) '(a b)) #t)
  (assert ((tree-1 'get) '(a b)) 2)
  (assert ((tree-1 'get) '(a)) '(directory ((b value)) #f))

  (assert ((tree-1 'set!) '(b a c d) 4) #t)
  (assert ((tree-1 'set!) '(b d d) 2) #t)
  (assert ((tree-1 'set!) '(b d e) 5) #t)
  (assert ((tree-1 'set!) '(b n c d) 1) #t)
  (assert ((tree-1 'copy!) '(b) '(b*)) #t)
  (assert ((tree-1 'prune!) '(b d d) #t) #t)
  (assert ((tree-1 'prune!) '(b d e)) #t)
  (assert ((tree-1 'get) '(b d)) '(directory ((d unknown)) #f))
  (assert ((tree-1 'get) '(b d d)) '(unknown))
  (assert ((tree-1 'get) '(b n c d)) 1)

  (assert ((tree-1 'equal?) '(b) '(b*)) #f)
  (assert ((tree-1 'equivalent?) '(b) '(b*)) #t)

  (assert ((tree-1 'valid?)) #t)

  (assert ((tree-2 'set!) '(a b) 2) #t)
  (assert ((tree-2 'set!) '(a* b) 4) #t)
  (assert ((tree-2 'prune!) '(a b)) #t)

  (assert ((tree-3 'set!) '(a b) 2) #t)
  (assert ((tree-3 'set!) '(a* b) 4) #t)
  (assert ((tree-3 'prune!) '(a* b)) #t)

  (assert ((tree-2 'merge!) (tree-3)) #t)
  (assert ((tree-2 'get) '(a b)) 2)
  (assert ((tree-2 'get) '(a* b)) 4)

  (append "Success (" (object->string asserted) " checks)"))
