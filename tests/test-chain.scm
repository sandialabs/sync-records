(lambda (standard-src chain-src)

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

  (define chain-1 (sync-eval (assert ((standard 'init) chain-src) sync-node?) #f))

  (assert ((chain-1 'size)) 0)

  (assert ((chain-1 'push!) (expression->byte-vector "hello")) #t)
  (assert ((chain-1 'push!) (expression->byte-vector ",")) #t)
  (assert ((chain-1 'push!) (expression->byte-vector "world")) #t)
  (assert ((chain-1 'push!) (expression->byte-vector "!")) #t)

  (assert (list (byte-vector->expression ((chain-1 'get) 0))
                (byte-vector->expression ((chain-1 'get) 1))
                (byte-vector->expression ((chain-1 'get) 2))
                (byte-vector->expression ((chain-1 'get) 3)))
          '("hello" "," "world" "!"))

  (let ((chain (sync-eval (chain-1) #f)))
    (assert ((chain 'set!) 1 (expression->byte-vector ":")) #t)
    (assert (byte-vector->expression ((chain 'get) 1)) ":"))

  (let ((chain (sync-eval (chain-1) #f)))
    (assert ((chain 'slice!) 1) #t)
    (assert (byte-vector->expression ((chain 'get) 1)) ","))

  (let ((chain (sync-eval (chain-1) #f)))
    (assert ((chain 'prune!) 1) #t)
    (assert (byte-vector->expression ((chain 'get) -1)) "!"))

  (let ((chain (sync-eval (chain-1) #f)))
    (assert ((chain 'truncate!) 1) (lambda (x) (sync-node? x)))
    (assert ((chain 'get) -1) (lambda (x) (byte-vector? x))))

  (let ((chain-1 (sync-eval ((standard 'init) chain-src) #f))
        (chain-2 (sync-eval ((standard 'init) chain-src) #f))
        (chain-3 (sync-eval ((standard 'init) chain-src) #f)))
    ((chain-1 'push!) (expression->byte-vector "hello"))
    ((chain-1 'push!) (expression->byte-vector ","))
    ((chain-1 'push!) (expression->byte-vector "world"))
    ((chain-1 'push!) (expression->byte-vector "!"))
    ((chain-2 'push!) (expression->byte-vector "hello"))
    ((chain-2 'push!) (expression->byte-vector ","))
    ((chain-2 'push!) (expression->byte-vector "world"))
    ((chain-2 'push!) (expression->byte-vector "!"))
    ((chain-3 'push!) (expression->byte-vector "hello"))
    ((chain-3 'push!) (expression->byte-vector ","))
    ((chain-3 'push!) (expression->byte-vector "world"))
    ((chain-3 'push!) (expression->byte-vector "!"))
    ((chain-2 'push!) (expression->byte-vector "this"))
    ((chain-2 'push!) (expression->byte-vector "is"))
    ((chain-3 'push!) (expression->byte-vector "this"))
    ((chain-3 'push!) (expression->byte-vector "is"))
    ((chain-3 'push!) (expression->byte-vector "a"))
    ((chain-3 'push!) (expression->byte-vector "somewhat"))
    ((chain-3 'push!) (expression->byte-vector "longer"))
    ((chain-3 'push!) (expression->byte-vector "chain"))
    (assert (and (equal? ((chain-1 'digest)) ((chain-2 'digest) -3))
                 (equal? ((chain-1 'digest)) ((chain-3 'digest) -7)))
            #t))

  (let ((chain (sync-eval ((standard 'init) chain-src) #f)))
    (let loop ((i 100))
      (if (= i 0)
          (assert ((chain 'size)) 100)
          (begin
            (assert ((chain 'push!) #u()) #t)
            (loop (- i 1))))))

  (let* ((size 100)
         (chain (sync-eval ((standard 'init) chain-src) #f))
         (digests-1 (let loop ((i 0) (digests '()))
                      (if (= i size) (reverse digests)
                          (begin
                            ((chain 'push!) (sync-null))
                            (loop (+ i 1) (cons ((chain 'digest)) digests))))))
         (digests-2 (let loop ((i 0) (digests '()))
                      (if (= i size) (reverse digests)
                          (loop (+ i 1) (cons ((chain 'digest) i) digests))))))
    (assert (equal? digests-1 digests-2) #t))

  (append "Success (" (object->string asserted) " checks)"))
