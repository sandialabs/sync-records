(lambda (standard-src)

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

  (assert (standard) (lambda (x) (sync-node? x)))

  (define foo-cls
    '(define-class (foo)
       (define-method (add self x) (+ x 1))
       (define-method (fact self x (result 1))
         (if (< x 1) result
             ((self 'fact) (- x 1) (* result x))))
       (define-method (state-get self)
         (byte-vector->expression (sync-cdr (self))))
       (define-method (state-set! self x)
         (set! (self) (sync-cons (sync-car (self))
                                 (expression->byte-vector x))))))

  (define foo-obj (assert ((standard 'make) foo-cls) sync-node?))
  (define foo (sync-eval foo-obj #f))

  (assert ((foo 'add) 1) 2)

  (assert ((foo 'fact) 4) (* 4 3 2 1))

  (assert ((foo 'state-set!) "hello, world!") #t)

  (assert ((foo 'state-get)) "hello, world!")

  (let* ((query '(lambda (node)
                   (let ((object (sync-eval node #f)))
                     ((object 'state-set!) 5)
                     ((object 'state-get)))))
         (serialized ((standard 'serialize) foo-obj query))
         (deserialized ((standard 'deserialize) serialized)))
    (assert ((eval query) deserialized) 5))

  (define class-1-cls
    '(define-class (class-1)
       (define-method (get self ~index)
         (self '(1)))
       (define-method (set! self ~index value)
         (set! (self '(1)) value))
       (define-method (slice! self ~index) #t)
       (define-method (prune! self ~index) #t)))

  (define class-2-cls
    '(define-class (class-2)
       (define-method (*init* self)
         (set! (self '(1)) (sync-cons (sync-null) (sync-null))))
       (define-method (get self index)
         (self `(1 ,index)))
       (define-method (set! self index value)
         (set! (self `(1 ,index)) value))
       (define-method (slice! self index)
         (let ((other (if (= index 0) 1 0)))
           (set! (self `(1 ,other)) (sync-cut (self `(1 ,other))))))
       (define-method (prune! self index)
         (set! (self `(1 ,index)) (sync-cut (self `(1 ,index)))))))

  (define class-3-cls
    '(define-class (class-3)
       (define-method (*init* self)
         (set! (self '(1)) (expression->byte-vector 0)))
       (define-method (get self ~key)
         (byte-vector->expression (self '(1))))
       (define-method (set! self ~key value)
         (set! (self '(1)) (expression->byte-vector value)))
       (define-method (increment! self)
         (let ((previous (byte-vector->expression (self '(1)))))
           (set! (self '(1)) (expression->byte-vector (+ previous 1)))))))

  (define object-1 (assert ((standard 'make) class-1-cls) sync-node?))

  (define object-2 (assert ((standard 'init) class-2-cls) sync-node?))

  (define object-3 (assert ((standard 'init) class-3-cls) sync-node?))

  (let ((object (sync-eval object-2 #f)))
    (assert ((object 'set!) 1 object-3) #t)
    (set! object-2 (object)))

  (let ((object (sync-eval object-1 #f)))
    (assert ((object 'set!) "some path" object-2) #t)
    (set! object-1 (object)))

  (assert ((standard 'deep-get) object-1 '("some path" 1 #f)) 0)

  (let* ((object object-1)
         (object ((standard 'deep-set!) object '("some path" 1 #f) 4)))
    (assert (sync-node? object) #t)
    (assert ((standard 'deep-get) object '("some path" 1 #f)) 4))

  (let* ((object object-1)
         (object ((standard 'deep-slice!) object '("some path" 1))))
    (assert (sync-node? object) #t)
    (assert ((standard 'deep-get) object '("some path" 1 #f)) 0))

  (let* ((object object-1)
         (object ((standard 'deep-prune!) object '("some path" 1))))
    (assert (sync-node? object) #t)
    (assert ((standard 'deep-get) object '("some path" 1)) '(unknown)))

  (let* ((object object-1)
         (object ((standard 'deep-copy!) object '("some path" 1) '("some path" 0))))
    (assert (sync-node? object) #t)
    (assert ((standard 'deep-get) object '("some path" 0 #f)) 0))

  (let* ((object object-1)
         (object ((standard 'deep-merge!) object object-1)))
    (assert (sync-node? object) #t)
    (assert ((standard 'deep-get) object '("some path" 1 #f)) 0))

  (let* ((object object-1)
         (object ((standard 'deep-call!) object '("some path" 1) '(lambda (obj) ((obj 'increment!))))))
    (assert (sync-node? object) #t)
    (let* ((object ((standard 'deep-call!) object '("some path" 1) '(lambda (obj) ((obj 'increment!))))))
      (assert (sync-node? object) #t)
      (assert ((standard 'deep-call) object '("some path" 1) '(lambda (obj) ((obj 'get) #f))) 2)))

  (append "Success (" (object->string asserted) " checks)"))
