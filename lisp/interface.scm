(macro (clear? secret-1 secret-2 window control standard chain tree ledger . classes)

  (if (or (not (string? secret-1)) (not (string? secret-2)))
      (error 'argument-error "Interface expects admin and interface secrets to be strings"))

  ;; install control logic
  (if clear?
      (sync-call `(,control ,secret-1 ,clear?) #t)
      (sync-call `(*eval* ,secret-1 (,control ,secret-1 ,clear?)) #t))

  (define (call function)
    (sync-call `(*call* ,secret-1 ,function) #t))

  (define (set-query function)
    (sync-call `(*set-query* ,secret-1 ,function) #t))

  (define (set-step function)
    (sync-call `(*set-step* ,secret-1 ,function) #t))

  ;; install and instantiate standard library
  (call `(lambda (root)
           (let ((init (caddr ,standard)))
             ((root 'set!) '(control class standard) ,standard)
             ((root 'set!) '(control object standard)
              ((eval `(lambda* ,(cddadr init) ,@(cddr init))) ,standard)))))

  ;; install required classes
  (call `(lambda (root) ((root 'set!) '(control class chain) ,chain)))
  (call `(lambda (root) ((root 'set!) '(control class tree) ,tree)))
  (call `(lambda (root) ((root 'set!) '(control class ledger) ,ledger)))

  ;; install optional classes
  (let loop ((classes classes))
    (if (null? classes) #t
        (begin (call `(lambda (root ((root 'set!) '(control object ,(caar classes)) ,(cadadr classes)))))
               (loop (cdr classes)))))

  (call `(lambda (root)
           (let* ((std-node ((root 'get) '(control object standard)))
                  (standard (sync-eval std-node #f))
                  (standard-class ((root 'get) '(control class standard)))
                  (tree-class ((root 'get) '(control class tree)))
                  (chain-class ((root 'get) '(control class chain)))
                  (ledger-class ((root 'get) '(control class ledger)))
                 (keys (crypto-generate (expression->byte-vector ,secret-2))))
             (if ,clear?
                 (let* ((config-expr `((public ((window ,,window) (public-key ,(car keys))))
                                       (private ((secret-key ,(cdr keys))))))
                        (ledger ((standard 'init) ledger-class std-node config-expr tree-class chain-class)))
                   ((root 'set!) '(control object ledger) ledger))
                 (let* ((ledger-old (sync-eval ((root 'get) '(control object ledger)) #f))
                        (ledger (sync-eval (sync-cons (sync-car ((standard 'make) ledger-class)) (ledger-old '(1))) #f))
                        (recode (lambda (class)
                                  (let ((code (sync-car ((standard 'make) class))))
                                    `(lambda (obj)
                                       (sync-cons ,code (sync-cdr obj)))))))
                   ((ledger 'update-config!) '(public window) ,window)
                   ((ledger 'update-config!) '(public public-key) (car keys))
                   ((ledger 'update-config!) '(private secret-key) (cdr keys))
                   ((ledger 'update-code!) 'standard (recode standard-class))
                   ((ledger 'update-code!) 'tree (recode tree-class))
                   ((ledger 'update-code!) 'chain (recode chain-class))
                   ((root 'set!) '(control object ledger) (ledger)))))))

  ;; define secret store
  (call `(lambda (root)
           ((root 'set!) '(interface secret) (sync-hash (expression->byte-vector ,secret-2)))))

  (define query-once
    '(lambda (query)
       (let* ((func (assoc 'function query))
              (args (assoc 'arguments query))
              (auth (assoc 'authentication query))
              (arg-list (if args (cadr args) '()))
              (keyword-args (let loop ((in (reverse arg-list)) (out '()))
                              (if (null? in) out
                                  (loop (cdr in) (append `(,(symbol->keyword (caar in)) ,(cadar in)) out)))))
              (std-node ((root 'get) '(control object standard)))
              (standard (sync-eval std-node #f))
              (node ((root 'get) '(control object ledger)))
              (ledger (sync-eval node #f)))

         ;; --- query helpers ---

         (define (~with-auth query)
           (append query `((authentication ,(cadr auth)))))

         (define (~self-call function args blocking?)
           (sync-call (~with-auth `((function ,function) (arguments ,args))) blocking?))

         (define (~authenticate)
           (if (not (equal? (sync-hash (expression->byte-vector (cadr auth))) ((root 'get) '(interface secret))))
               (error 'authentication-error "Could not authenticate restricted interface call")))

         (define (~bridge-path? path)
           (and (> (length path) 1)
                (pair? (cadr path))
                (eq? (caadr path) '*bridge*)))

         (define (~bridge-chain-path? path)
           (and (~bridge-path? path)
                (> (length (cadr path)) 1)))

         ;; --- remote bridge helpers ---

         (define (~fetch-remote-head local-chain path)
           (let* ((name (cadadr path))
                  (interface ((ledger 'config) `(private bridge ,name interface)))
                  (local-index (car path))
                  (remote-chain ((standard 'deep-get) local-chain `(,local-index (*bridge* ,name chain))))
                  (remote-index (- (((sync-eval remote-chain #f) 'size)) 1))
                  (remote-path (list-tail path 2))
                  (query `((function trace) (arguments ((index ,remote-index) (path ,remote-path)))))
                  (response (sync-remote interface query))
                  (head ((standard 'deserialize) response)))
             (if (and (not (~bridge-chain-path? remote-path))
                      (not (equal? (sync-digest remote-chain) (sync-digest head))))
                 (error 'digest-error "Remote chain does not match local chain head")
                 head)))

         (define* (~fetch-merged-head path (index -1))
           (let* ((chain ((ledger 'resolve) '()))
                  (local-chain (((sync-eval chain #f) 'previous) index))
                  (head (~fetch-remote-head local-chain path))
                  (prefix (reverse (list-tail (reverse path) (- (length path) 2)))))
             ((standard 'deep-merge!) head local-chain prefix)))

         ;; --- handlers ---

         (define* (*secret* (secret (error 'arg-error "Missing arg: secret")))
           ;; Set the interface authentication secret.
           ;;   Args:
           ;;     secret (string): new interface secret.
           ;;   Returns:
           ;;     sync hash: stored secret hash.
           ((root 'set!) '(interface secret) (sync-hash (expression->byte-vector secret))))

         (define* (resolve (path (error 'arg-error "Missing arg: path")) pinned? proof? head)
           ;; Resolve a path, optionally using a provided proof head or remote bridge fetch.
           ;;   Args:
           ;;     path (list): target path.
           ;;     pinned? (boolean): #t to prefer pinned history.
           ;;     proof? (boolean): #t to return proof-ish result.
           ;;     head (sync node): optional pre-fetched chain head.
           ;;   Returns:
           ;;     any: resolved value or sentinel.
           (if head ((ledger 'resolve) path pinned? proof? head)
               (let ((attempt ((ledger 'resolve) path)))
                 (cond ((not (equal? attempt '(unknown))) ((ledger 'resolve) path pinned? proof?))
                       ((not (~bridge-path? path)) attempt)
                       ((~bridge-chain-path? path)
                        ((ledger 'resolve) path pinned? proof? (~fetch-merged-head path)))
                       (else ((ledger 'resolve) path pinned? proof? (~fetch-merged-head path)))))))

         (define* (trace (index (error 'arg-error "Missing arg: index")) (path (error 'arg-error "Missing arg: path")) head)
           ;; Return a proof trace for a path, fetching remote bridge state when needed.
           ;;   Args:
           ;;     index (integer): trace index.
           ;;     path (list): target path.
           ;;     head (sync node): optional pre-fetched chain head.
           ;;   Returns:
           ;;     sync node: traced proof.
           (if head ((ledger 'trace) index path head)
               (let ((attempt ((ledger 'resolve) (cond ((null? path) `(,index))
                                                       ((>= (car path) 0) path)
                                                       ((>= index 0) (cons (+ (+ index 1) (car path)) (cdr path)))
                                                       (else (cons (+ index (car path) 1) (cdr path)))))))
                 (cond ((not (equal? attempt '(unknown))) ((ledger 'trace) index path))
                       ((not (~bridge-path? path)) (error 'trace-error "Unknown path trace"))
                       (else ((ledger 'trace) index path (~fetch-merged-head path index)))))))

         (define* (pin! (path (error 'arg-error "Missing arg: path")) response)
           ;; Pin content locally, fetching and reserializing remote bridge content when needed.
           ;;   Args:
           ;;     path (list): target path.
           ;;     response (expression): optional serialized pin response.
           ;;   Returns:
           ;;     boolean: #t after pinning.
           (if response ((ledger 'pin!) path response)
               (let ((attempt ((ledger 'resolve) path)))
                 (cond ((not (equal? attempt '(unknown))) ((ledger 'pin!) path))
                       ((not (~bridge-path? path)) (error 'pin-error "Cannot pin unknown content"))
                       (else (let* ((merged (~fetch-merged-head path))
                                    (proof ((standard 'deep-slice!) merged path))
                                    (response ((standard 'serialize) proof))
                                    (args (append arg-list `((response ,response)))))
                               (~self-call 'pin! args #f)))))))

         (define* (bridge! (name (error 'arg-error "Missing arg: name"))
                           (interface (error 'arg-error "Missing arg: interface"))
                           info)
           ;; Register a bridge and lazily fetch remote info when omitted.
           ;;   Args:
           ;;     name (symbol): bridge name.
           ;;     interface (string): remote interface.
           ;;     info (expression): optional remote info payload.
           ;;   Returns:
           ;;     boolean: #t after bridge registration.
           (if info ((ledger 'bridge!) name interface info)
               (let* ((info (sync-remote interface '((function info))))
                      (args `((name ,name) (interface ,interface) (info ,info))))
                 (~self-call 'bridge! args #t))))

         (define (~method)
           (let ((result (apply (ledger (cadr func)) keyword-args)))
             result))

         ;; --- dispatch ---

         (let ((ret (case (cadr func)
                      ((*secret*) (~authenticate) (apply *secret* keyword-args))
                      ((resolve) (~authenticate) (apply resolve keyword-args))
                      ((trace) (apply trace keyword-args))
                      ((pin!) (~authenticate) (apply pin! keyword-args))
                      ((bridge!) (~authenticate) (apply bridge! keyword-args))
                      ((set! set-batch! unpin!) (~authenticate) (~method))
                      ((size synchronize info config get) (~method))
                      (else (error 'api-error "Interface does not implement API endpoint")))))
           ((root 'set!) '(control object ledger) (ledger))
           ret))))

  (set-query
   `(lambda (root query)
      (let ((query-once ,query-once))
        (if (not (eq? (cadr (assoc 'function query)) 'batch!)) (query-once query)
            (let ((auth (assoc 'authentication query)))
              (let loop ((queries (cadr (assoc 'queries (cadr (assoc 'arguments query))))) (result '()))
                (if (null? queries) (reverse result)
                    (let ((subquery (if auth (append (car queries) auth) (car queries)))) 
                      (loop (cdr queries) (cons (query-once subquery) result))))))))))
  (define step-once
    '(lambda (root secret query)
       (let* ((query (if (null? query) '(ledger-step) query))
              (std-node ((root 'get) '(control object standard)))
              (standard (sync-eval std-node #f))
              (node ((root 'get) '(control object ledger)))
              (ledger (sync-eval node #f)))

         ;; --- query helpers ---

         (define (~self-call blocking? query)
           (sync-call `(*step* ,secret ,query) blocking?))

         ;; --- handlers ---

         (define* (bridge-synchronize! (name (error 'arg-error "Missing arg: name")) index response)
           ;; Synchronize a bridge, optionally using a provided remote response.
           ;;   Args:
           ;;     name (symbol): bridge name.
           ;;     index (integer): optional last synchronized index.
           ;;     response (expression): optional serialized sync response.
           ;;   Returns:
           ;;     boolean: #t after synchronization.
           (if (and index response) ((ledger 'bridge-synchronize!) name index response)
               (let* ((last ((ledger 'get) `((*bridge* ,name chain))))
                      (index (if (sync-node? last) (- (((sync-eval last #f) 'size)) 1) -1))
                      (interface ((ledger 'config) `(private bridge ,name interface)))
                      (response (sync-remote interface `((function synchronize) (arguments ((index ,index)))))))
                 (~self-call #t `(bridge-synchronize! ,name ,index ,response)))))

         (define* (ledger-step mutate?)
           ;; Run one interface step, optionally mutating the local ledger at the end.
           ;;   Args:
           ;;     mutate? (boolean): #t to perform the final local step.
           ;;   Returns:
           ;;     integer: resulting ledger size.
           (if mutate? (begin ((ledger 'step!) (system-time-unix)) ((ledger 'size)))
               (let loop ((names (map car ((ledger 'config) '(private bridge)))))
                 (if (null? names)
                     (~self-call #t '(ledger-step #t))
                     (begin
                       (~self-call #f `(bridge-synchronize! ,(car names)))
                       (loop (cdr names)))))))

         ;; --- dispatch ---

         (let ((ret (case (car query)
                      ((ledger-step) (apply ledger-step (cdr query)))
                      ((bridge-synchronize!) (apply bridge-synchronize! (cdr query)))
                      (else (error 'api-error "Step does not implement operation")))))
           ((root 'set!) '(control object ledger) (ledger))
           ret))))

  (set-step step-once)

  "Installed interface")
