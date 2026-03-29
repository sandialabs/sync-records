(lambda (standard-src chain-src tree-src ledger-src)

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

  (define chain-cls chain-src)
  (define tree-cls tree-src)
  (define ledger-cls ledger-src)

  (define (make-ledger name)
    (let* ((keys (crypto-generate (expression->byte-vector name)))
           (config-expr `((public ((window 4)
                                   (public-key ,(car keys))))
                          (private ((secret-key ,(cdr keys)))))))
      (assert ((standard 'init) ledger-cls (standard) config-expr tree-cls chain-cls)
              sync-node?)))

  (define ledger-1 (sync-eval (make-ledger 'ledger-1) #f))
  (define ledger-2 (sync-eval (make-ledger 'ledger-2) #f))
  (define ledger-3 (sync-eval (make-ledger 'ledger-3) #f))
  (define ledger-4 (sync-eval (make-ledger 'ledger-4) #f))
  (define ledger-5 (sync-eval (make-ledger 'ledger-5) #f))

  (define (step-all! ledger)
    (let loop ((names (map car ((ledger 'config) '(private bridge))))
               (result (list ((ledger 'step!) (system-time-unix)))))
      (if (null? names) (reverse result)
          (let* ((name (car names))
                 (peer (eval name))
                 (value ((ledger 'get) `((*bridge* ,name chain))))
                 (index (if (sync-node? value) (- (((sync-eval value #f) 'size)) 1) -1))
                 (response ((peer 'synchronize) index)))
            (loop (cdr names)
                  (cons ((ledger 'bridge-synchronize!) name index response) result))))))

  (define (bridge! ledger peer-name)
    (let ((peer (eval peer-name)))
      ((ledger 'bridge!) peer-name peer-name ((peer 'info)))))

  (define (serialize-full node)
    ((standard 'serialize) node
     '(lambda (x)
        (let loop ((x x))
          (if (not (sync-pair? x)) x
              (sync-cons (loop (sync-car x)) (loop (sync-cdr x))))))))

  (define* (fetch ledger path index)
    (let* ((name (cadadr path))
           (peer (eval name))
           (local-index (car path))
           (local-chain ((ledger 'resolve) '()))
           (local-chain (if index (((sync-eval local-chain #f) 'previous) index) local-chain))
           (remote-chain ((ledger 'resolve) `(,local-index (*bridge* ,name chain))))
           (remote-index (- (((sync-eval remote-chain #f) 'size)) 1))
           (remote-path (list-tail path 2))
           (response (trace peer remote-index remote-path))
           (head ((standard 'deserialize) response))
           (prefix (reverse (list-tail (reverse path) (- (length path) 2))))
           (merged ((standard 'deep-merge!) head remote-chain)))
      (if (not (equal? (sync-digest remote-chain) (sync-digest head)))
          (error 'digest-error "Remote chain does not match local chain head")
          ((standard 'deep-set!) local-chain prefix merged))))

  (define (bridge-chain-path? path)
    (and (> (length path) 1)
         (pair? (cadr path))
         (eq? (caadr path) '*bridge*)
         (> (length (cadr path)) 1)))

  (define* (resolve ledger path pinned? proof?)
    (let ((attempt ((ledger 'resolve) path)))
      (cond ((not (equal? attempt '(unknown))) ((ledger 'resolve) path pinned? proof?))
            ((or (< (length path) 2) (not (pair? (cadr path))) (not (eq? (caadr path) '*bridge*))) attempt)
            (else ((ledger 'resolve) path pinned? proof? (fetch ledger path))))))

  (define (trace ledger index path)
    (let* ((path~ (cond ((null? path) `(,index))
                        ((>= (car path) 0) path)
                        ((>= index 0) (cons (+ (+ index 1) (car path)) (cdr path)))
                        (else (cons (+ index (car path) 1) (cdr path)))))
           (attempt ((ledger 'resolve) path~)))
      (cond ((not (equal? attempt '(unknown))) ((ledger 'trace) index path))
            ((or (< (length path~) 2) (not (pair? (cadr path~))) (not (eq? (caadr path~) '*bridge*)))
             (error 'trace-error "Unknown path trace"))
            (else ((ledger 'trace) index path (fetch ledger path~ index))))))

  (define (pin! ledger path)
    (let ((attempt ((ledger 'resolve) path)))
      (cond ((not (equal? attempt '(unknown))) ((ledger 'pin!) path #f))
            ((not (eq? (caadr path) '*bridge*))
             (error 'pin-error "Cannot pin unknown content"))
            (else
             (let ((response (serialize-full (fetch ledger path))))
               ((ledger 'pin!) path response))))))

  (assert ((ledger-1 'config)) (lambda (x) (and (list? x) (not (null? x)))))

  (assert ((ledger-1 'size)) 0)

  (assert (step-all! ledger-1) '(1))

  (assert ((ledger-1 'get) '((*state*))) '(directory ((*time* value)) #t))

  (assert ((ledger-1 'set!) '((*state* do pin this)) "yes") #t)
  (assert ((ledger-1 'set!) '((*state* do pin that)) "yes") #t)
  (assert ((ledger-1 'set!) '((*state* do not pin)) "no") #t)
  (assert ((ledger-1 'set-batch!)
           '(((*state* batch alpha))
             ((*state* batch beta)))
           '("a" "b")) #t)
  (assert ((ledger-1 'get) '((*state* do not pin))) "no")
  (assert ((ledger-1 'get) '((*state* do pin))) '(directory ((this value) (that value)) #t))
  (assert ((ledger-1 'get) '((*state* batch alpha))) "a")
  (assert ((ledger-1 'get) '((*state* batch beta))) "b")

  (assert (step-all! ledger-1) '(2))

  (assert (resolve ledger-1 '(-1 (*state* do pin this)) #f #f) "yes")
  (assert (resolve ledger-1 '(-1 (*state* do pin that)) #f #f) "yes")

  (assert (pin! ledger-1 '(1 (*state* do pin this))) #t)
  (assert (pin! ledger-1 '(1 (*state* do pin that))) #t)

  (assert (resolve ledger-1 '(1 (*state* do pin)) #f #f)
          '(directory ((this value) (that value)) #t))

  (assert (bridge! ledger-1 'ledger-2) #t)
  (assert (step-all! ledger-1) (lambda (x) (and (list? x) (= (car x) 2))))

  (assert ((ledger-2 'set!) '((*state* a b c)) 42) #t)
  (assert ((ledger-2 'set!) '((*state* a b c*)) 43) #t)
  (assert (step-all! ledger-2) '(1))
  (assert (resolve ledger-2 '(-1 (*state* a b c*)) #f #f) 43)

  (assert (step-all! ledger-1) (lambda (x) (and (list? x) (= (car x) 3))))
  (assert (step-all! ledger-1) (lambda (x) (and (list? x) (= (car x) 4))))

  (assert (resolve ledger-1 '(-1 (*bridge*)) #f #f)
          '(directory ((ledger-2 directory)) #t))

  (assert (resolve ledger-1 '(-1 (*bridge* ledger-2 chain) -1 (*state* a b)) #f #f)
          '(directory ((c value) (c* value)) #t))

  (assert (resolve ledger-1 '(-1 (*bridge* ledger-2 chain) -1 (*state* a b c)) #f #f)
          42)

  (assert (pin! ledger-1 '(-1 (*bridge* ledger-2 chain) -1 (*state* a b c))) #t)

  (assert (resolve ledger-1 '(-1 (*bridge* ledger-2 chain) -1 (*state* a b c)) #t #f)
          (lambda (x) (and (equal? (cadr (assoc 'content x)) 42)
                           (equal? (cadr (assoc 'pinned? x)) #t))))

  (assert (bridge! ledger-2 'ledger-3) #t)
  (assert (bridge! ledger-3 'ledger-4) #t)
  (assert (bridge! ledger-3 'ledger-5) #t)

  (assert ((ledger-3 'set!) '((*state* d e f)) 64) #t)
  (assert ((ledger-4 'set!) '((*state* g h i)) "hello") #t)
  (assert ((ledger-5 'set!) '((*state* g h i)) "world") #t)

  (assert (step-all! ledger-3) (lambda (x) (and (list? x) (= (car x) 1))))
  (assert (step-all! ledger-4) '(1))
  (assert (step-all! ledger-5) '(1))

  (assert (step-all! ledger-2) '(1 #t))
  (assert (step-all! ledger-1) '(4 #t))

  (assert (step-all! ledger-2) '(2 #t))
  (assert (step-all! ledger-1) '(4 #t))

  (assert (resolve ledger-2 '(-1 (*bridge* ledger-3 chain) -1 (*state* d e f)) #f #f) 64)

  (assert (step-all! ledger-2) '(2 #t))
  (assert (step-all! ledger-1) '(5 #t))

  (assert (resolve ledger-1 '(-1 (*bridge* ledger-2 chain) -1
                                 (*bridge* ledger-3 chain) -1
                                 (*state* d e f)) #f #f)
          64)

  (assert (step-all! ledger-1) '(5 #t))
  (assert (step-all! ledger-2) '(2 #t))
  (assert (step-all! ledger-3) '(2 #t #t))
  (assert (step-all! ledger-4) '(1))
  (assert (step-all! ledger-5) '(1))

  (assert (step-all! ledger-1) '(5 #t))
  (assert (step-all! ledger-2) '(2 #t))
  (assert (step-all! ledger-3) '(3 #t #t))
  (assert (step-all! ledger-4) '(1))
  (assert (step-all! ledger-5) '(1))

  (assert (step-all! ledger-1) '(5 #t))
  (assert (step-all! ledger-2) '(3 #t))
  (assert (step-all! ledger-3) '(3 #t #t))
  (assert (step-all! ledger-4) '(1))
  (assert (step-all! ledger-5) '(1))

  (assert (step-all! ledger-1) '(5 #t))
  (assert (step-all! ledger-2) '(4 #t))
  (assert (step-all! ledger-3) '(3 #t #t))
  (assert (step-all! ledger-4) '(1))
  (assert (step-all! ledger-5) '(1))

  (assert (step-all! ledger-1) '(6 #t))
  (assert (step-all! ledger-2) '(4 #t))
  (assert (step-all! ledger-3) '(3 #t #t))
  (assert (step-all! ledger-4) '(1))
  (assert (step-all! ledger-5) '(1))

  (assert (step-all! ledger-1) '(7 #t))
  (assert (step-all! ledger-2) '(4 #t))
  (assert (step-all! ledger-3) '(3 #t #t))
  (assert (step-all! ledger-4) '(1))
  (assert (step-all! ledger-5) '(1))

  (assert (resolve ledger-1 '(-1 (*bridge* ledger-2 chain) -1
                                 (*bridge* ledger-3 chain) -1
                                 (*bridge* ledger-4 chain) -1
                                 (*state* g h i)) #f #f)
          "hello")

  (assert (resolve ledger-1 '(-1 (*bridge* ledger-2 chain) -1
                                 (*bridge* ledger-3 chain) -1
                                 (*bridge* ledger-5 chain) -1
                                 (*state* g h i)) #t #f)
          (lambda (x) (equal? (cadr (assoc 'content x)) "world")))

  (assert ((ledger-1 'set!) '((*state* tick)) 0) #t)
  (assert (step-all! ledger-1) '(8 #t))

  (assert (resolve ledger-1 '(1 (*state* do pin)) #f #f)
          '(directory ((this value) (that value)) #t))
  (assert (resolve ledger-1 '(1 (*state* do pin this)) #f #f) "yes")
  (assert (resolve ledger-1 '(1 (*state* do pin that)) #f #f) "yes")
  (assert (resolve ledger-1 '(1 (*state* do not pin)) #f #f) '(unknown))

  (assert ((ledger-1 'unpin!) '(1 (*state* do pin that))) #t)
  (assert (resolve ledger-1 '(1 (*state* do pin this)) #f #f) "yes")
  (assert (resolve ledger-1 '(1 (*state* do pin that)) #f #f) '(unknown))

  (assert ((ledger-1 'update-config!) '(public public-key) #u(0)) #t)
  (assert ((ledger-1 'update-config!) '(private secret-key) #u(1)) #t)
  (assert ((ledger-1 'update-config!) '(public window) 2) #t)
  (assert ((ledger-1 'update-code!) 'tree '(lambda (obj) obj)) #t)
  (assert ((ledger-1 'update-code!) 'chain '(lambda (obj) obj)) #t)

  (append "Success (" (object->string asserted) " checks)"))
