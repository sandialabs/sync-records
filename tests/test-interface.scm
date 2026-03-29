(lambda (control-src standard-src chain-src tree-src ledger-src interface-src)

  (define asserted 0)

  (define interface-src
    (let recurse ((src interface-src))
      (cond ((or (not (list? src)) (null? src)) src)
            ((equal? (car src) 'sync-remote)
             `(sync-call (append ,(caddr src) (list (list 'authentication ,(cadr src))))
                         #t
                         (sync-hash (expression->byte-vector ,(cadr src)))))
            ((equal? (car src) 'sync-call)
             (append `(sync-call ,(cadr src) #t) (cdddr src)))
            ((list? (car src))
             (cons (recurse (car src)) (recurse (cdr src))))
            (else (cons (car src) (recurse (cdr src)))))))

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

  (define (journal-id name)
    (sync-hash (expression->byte-vector name)))

  (define (journal-install journal admin-secret interface-secret)
    (sync-call `(,interface-src #t ,admin-secret ,interface-secret 4
                                ,control-src ',standard-src ',chain-src ',tree-src ',ledger-src)
               #t journal))

  (define (journal-query journal query)
    (sync-call query #t journal))

  (define (interface-query journal interface query)
    (journal-query journal (append query `((authentication ,interface)))))

  (define interface-1 "http://journal-1.test/interface")
  (define interface-2 "http://journal-2.test/interface")
  (define interface-3 "http://journal-3.test/interface")
  (define interface-4 "http://journal-4.test/interface")
  (define interface-5 "http://journal-5.test/interface")
  (define journal-1 (journal-id interface-1))
  (define journal-2 (journal-id interface-2))
  (define journal-3 (journal-id interface-3))
  (define journal-4 (journal-id interface-4))
  (define journal-5 (journal-id interface-5))

  (sync-create journal-1)
  (sync-create journal-2)
  (sync-create journal-3)
  (sync-create journal-4)
  (sync-create journal-5)

  (assert (journal-install journal-1 "pass-1" interface-1) "Installed interface")
  (assert (journal-install journal-2 "pass-2" interface-2) "Installed interface")
  (assert (journal-install journal-3 "pass-3" interface-3) "Installed interface")
  (assert (journal-install journal-4 "pass-4" interface-4) "Installed interface")
  (assert (journal-install journal-5 "pass-5" interface-5) "Installed interface")

  (let ((query '((function size))))
    (assert (journal-query journal-1 query) 0))

  (let ((query '((function set!) (arguments ((path ((*state* hello))) (value "world"))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query '((function get) (arguments ((path ((*state* hello))))))))
    (assert (interface-query journal-1 interface-1 query) "world"))

  (let ((query '((function set-batch!)
                 (arguments ((paths (((*state* batch alpha)) ((*state* batch beta))))
                             (values ("a" "b")))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query '((function get) (arguments ((path ((*state* batch alpha))))))))
    (assert (interface-query journal-1 interface-1 query) "a"))

  (let ((query '((function get) (arguments ((path ((*state* batch beta))))))))
    (assert (interface-query journal-1 interface-1 query) "b"))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-1 interface-1 query) 1))

  (let ((query '((function resolve) (arguments ((path (-1 (*state* hello))) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) "world"))

  (let ((query '((function set!) (arguments ((path ((*state* do pin this))) (value "yes"))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query '((function set!) (arguments ((path ((*state* do pin that))) (value "yes"))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query '((function set!) (arguments ((path ((*state* do not pin))) (value "no"))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-1 interface-1 query) 2))

  (let ((query '((function pin!) (arguments ((path (1 (*state* do pin this))))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query '((function pin!) (arguments ((path (1 (*state* do pin that))))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query `((function bridge!) (arguments ((name journal-2) (interface ,interface-2))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query '((function set!) (arguments ((path ((*state* a b c))) (value 42))))))
    (assert (interface-query journal-2 interface-2 query) #t))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-2 interface-2 query) 1))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-1 interface-1 query) 3))

  (let* ((path '(-1 (*bridge* journal-2 chain) -1 (*state* a b c)))
         (query `((function resolve) (arguments ((path ,path) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) 42))

  (let ((query '((function resolve) (arguments ((path (-1 (*bridge*))) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) '(directory ((journal-2 directory)) #t)))

  (let* ((path '(-1 (*bridge* journal-2 chain) -1 (*state* a b)))
         (query `((function resolve) (arguments ((path ,path) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) '(directory ((c value)) #t)))

  (let ((query `((function bridge!) (arguments ((name journal-3) (interface ,interface-3))))))
    (assert (interface-query journal-2 interface-2 query) #t))

  (let ((query `((function bridge!) (arguments ((name journal-4) (interface ,interface-4))))))
    (assert (interface-query journal-3 interface-3 query) #t))

  (let ((query `((function bridge!) (arguments ((name journal-5) (interface ,interface-5))))))
    (assert (interface-query journal-3 interface-3 query) #t))

  (let ((query '((function set!) (arguments ((path ((*state* d e f))) (value 64))))))
    (assert (interface-query journal-3 interface-3 query) #t))

  (let ((query '((function set!) (arguments ((path ((*state* g h i))) (value "hello"))))))
    (assert (interface-query journal-4 interface-4 query) #t))

  (let ((query '((function set!) (arguments ((path ((*state* g h i))) (value "world"))))))
    (assert (interface-query journal-5 interface-5 query) #t))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-3 interface-3 query) 1))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-4 interface-4 query) 1))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-5 interface-5 query) 1))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-3 interface-3 query) 2))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-2 interface-2 query) 2))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-1 interface-1 query) 4))

  (let* ((path '(-1 (*bridge* journal-3 chain) -1 (*state* d e f)))
         (query `((function resolve) (arguments ((path ,path) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-2 interface-2 query) 64))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-2 interface-2 query) 2))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-1 interface-1 query) 4))

  (let* ((path '(-1 (*bridge* journal-2 chain) -1 (*bridge* journal-3 chain) -1 (*state* d e f)))
         (query `((function resolve) (arguments ((path ,path) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) 64))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-2 interface-2 query) 2))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-1 interface-1 query) 4))

  (let* ((path '(-1 (*bridge* journal-2 chain) -1 (*bridge* journal-3 chain) -1 (*bridge* journal-4 chain) -1 (*state* g h i)))
         (query `((function resolve) (arguments ((path ,path) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) "hello"))

  (let* ((path '(-1 (*bridge* journal-2 chain) -1 (*bridge* journal-3 chain) -1 (*bridge* journal-5 chain) -1 (*state* g h i)))
         (query `((function resolve) (arguments ((path ,path) (pinned? #t) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) (lambda (x) (equal? (cadr (assoc 'content x)) "world"))))

  (let ((query '((function set!) (arguments ((path ((*state* tick))) (value 0))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-1 interface-1 query) 5))

  (let ((query '((function set!) (arguments ((path ((*state* tick))) (value 1))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-1 interface-1 query) 6))

  (let ((query '((function set!) (arguments ((path ((*state* tick))) (value 2))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-1 interface-1 query) 7))

  (let ((query '((function set!) (arguments ((path ((*state* tick))) (value 3))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query '((function *step!*))))
    (assert (interface-query journal-1 interface-1 query) 8))

  (let ((query '((function resolve) (arguments ((path (1 (*state* do pin))) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) '(directory ((this value) (that value)) #t)))

  (let ((query '((function resolve) (arguments ((path (1 (*state* do pin this))) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) "yes"))

  (let ((query '((function resolve) (arguments ((path (1 (*state* do pin that))) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) "yes"))

  (let ((query '((function resolve) (arguments ((path (1 (*state* do not pin))) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) '(unknown)))

  (let ((query '((function unpin!) (arguments ((path (1 (*state* do pin that))))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query '((function resolve) (arguments ((path (1 (*state* do pin this))) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) "yes"))

  (let ((query '((function resolve) (arguments ((path (1 (*state* do pin that))) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) '(unknown)))

  (let ((query '((function *secret*) (arguments ((secret "pass-1-new"))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let* ((expr `(,interface-src #f "pass-1" ,interface-1 2 ,control-src ',standard-src ',chain-src ',tree-src ',ledger-src))
         (query `(*eval* "pass-1" ,expr)))
    (assert (journal-query journal-1 query) "Installed interface"))

  (let ((query '((function size))))
    (assert (journal-query journal-1 query) 8))

  (let ((query '((function config) (arguments ((path (public window)))))))
    (assert (interface-query journal-1 interface-1 query) 2))

  (let ((query '((function get) (arguments ((path ((*state* hello))))))))
    (assert (interface-query journal-1 interface-1 query) "world"))

  (let ((query '((function resolve) (arguments ((path (-1 (*state* hello))) (pinned? #f) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) "world"))

  (let ((query '((function pin!) (arguments ((path (-1 (*state* hello))))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (let ((query '((function resolve) (arguments ((path (-1 (*state* hello))) (pinned? #t) (proof? #f))))))
    (assert (interface-query journal-1 interface-1 query) '((content "world") (pinned? #t))))

  (let ((query '((function unpin!) (arguments ((path (-1 (*state* hello))))))))
    (assert (interface-query journal-1 interface-1 query) #t))

  (append "Success (" (object->string asserted) " checks)"))
