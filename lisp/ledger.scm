(define-class (ledger)
  ;; Ledger class manages config, staged state, and signed/pinned chain history.
  (define-method (~field self name value)
    ;; Resolve or set internal field by name.
    ;;   Args:
    ;;     name (symbol): field name.
    ;;     value (optional procedure returning value or #f): thunk to store, or #f to read.
    ;;   Returns:
    ;;     any: field value when value is #f, otherwise #t after set.
    (let ((address (case name
                     ((standard) '(1 0 0 0))
                     ((config) '(1 0 0 1))
                     ((stage) '(1 0 1 0))
                     ((temp) '(1 0 1 1))
                     ((perm) '(1 1 1))
                     (else 'case-error "Field not found"))))
      (if value (set! (self address) (value))
          (let ((node (self address)))
            ((eval (byte-vector->expression (sync-car node))) node)))))

  (define-method (~path-check self path bridge-okay?)
    ;; Validate a state/bridge path shape before access.
    ;;   Args:
    ;;     path (list): path segments.
    ;;     bridge-okay? (boolean): allow *bridge* paths.
    ;;   Returns:
    ;;     boolean: #t when valid (or raises on invalid path).
    (cond ((not (list? (car path)))
           (error 'path-error "Expected a list in the next path element"))
          ((and (not bridge-okay?) (eq? (caar path) '*bridge*))
           (error 'path-error "Expected the first element in the next path element to be '*state*"))
          ((not (or (eq? (caar path) '*state*) (eq? (caar path) '*bridge*)))
           (error 'path-error "Expected the first element in the next path element to be '*state* or '*bridge*"))
          (else #t)))

  (define-method (~signature-sign! self chain)
    ;; Embed public key and signature into chain head using config keys.
    ;;   Args:
    ;;     chain (chain object): chain to sign.
    ;;   Returns:
    ;;     boolean: #t after mutating chain.
    (let* ((config ((self '~field) 'config))
           (standard ((self '~field) 'standard))
           (public-key ((config 'get) '(public public-key)))
           (secret-key ((config 'get) '(private secret-key))))
      ((standard 'deep-set!) chain '(-1 (*crypto* public-key)) #u())
      ((standard 'deep-set!) chain '(-1 (*crypto* signature)) #u())
      ((standard 'deep-call!) chain '(-1)
       (lambda (tree)
         ((tree 'set!) '(*crypto* public-key) public-key)
         ((tree 'set!) '(*crypto* signature) (crypto-sign secret-key (sync-digest (chain))))))))

  (define-method (~signature-verify self chain public-key)
    ;; Verify chain head signature and optional expected public key.
    ;;   Args:
    ;;     chain (chain object): chain to verify.
    ;;     public-key (byte-vector or #f): expected public key.
    ;;   Returns:
    ;;     boolean: #t when signature verifies (or raises on failure).
    (let* ((config ((self '~field) 'config))
           (standard ((self '~field) 'standard))
           (chain-copy ((standard 'load) (chain))))
      ((standard 'deep-set!) chain-copy '(-1 (*crypto* public-key)) #u())
      ((standard 'deep-set!) chain-copy '(-1 (*crypto* signature)) #u())
      ((standard 'deep-call!) chain '(-1)
       (lambda (tree)
         (let ((included-key ((tree 'get) '(*crypto* public-key)))
               (signature ((tree 'get) '(*crypto* signature))))
           (cond ((or (null? signature) (null? included-key))
                  (error 'signature-error "Chain doesn not include necessary public key and signature"))
                 ((and public-key (not (equal? public-key included-key)))
                  (error 'signature-error "Included key does not match expected public key"))
                 ((not (crypto-verify public-key signature (sync-digest (chain-copy))))
                  (error 'signature-error "Included signature does not verify"))
                 (else #t)))))))

  (define-method (*init* self standard config tree-class chain-class)
    ;; Initialize ledger with a standard API and configuration object.
    ;;   Args:
    ;;     standard (standard class object): standard helper instance.
    ;;     config (configuration object): configuration expression.
    ;;   Returns:
    ;;     boolean: #t after setting fields.
    ((self '~field) 'standard standard)
    ((self '~field) 'config config)
    ((self '~field) 'stage ((standard 'make) tree-class))
    ((self '~field) 'temp ((standard 'make) chain-class))
    ((self '~field) 'perm ((standard 'make) chain-class)))

  (define-method (configuration self (path '()))
    ;; Return full configuration data (private and public).
    ;;   Returns:
    ;;     list: configuration expression.
    ((((self '~field) 'config) 'get) path))

  (define-method (information self)
    ;; Return public configuration information only.
    ;;   Returns:
    ;;     list: public configuration expression.
    ((((self '~field) 'config) 'get) '(public)))

  (define-method (bridges self)
    ;; Return configured bridge names.
    ;;   Returns:
    ;;     list: bridge names.
    (let ((entries ((((self '~field) 'config) 'get) '(private bridge))))
      (map car entries)))

  (define-method (size self)
    ;; Size of the permanent chain.
    ;;   Returns:
    ;;     integer: chain size.
    (let ((perm ((self '~field) 'perm)))
      ((perm 'size))))

  (define-method (bridge! self name info)
    ;; Register bridge info and cache its public key.
    ;;   Args:
    ;;     name (symbol): bridge name.
    ;;     info (alist or expression with `information`): bridge info.
    ;;   Returns:
    ;;     boolean: #t after updating config.
    ;; todo: reach out and ask bridge for public key (maybe? need a "config" endpoint)
    (let ((config ((self '~field) 'config)))
      ((config 'set!) `(private bridge ,name) info)
      ((config 'set!) `(private bridge ,name public-key)
       (cadr (assoc 'public-key ((eval (cadr (assoc 'information info)))))))
      ((self '~field) 'config config)))

  (define-method (set! self path value)
    ;; Stage a state change at path (not yet committed).
    ;;   Args:
    ;;     path (list): path segments.
    ;;     value (any): value to set.
    ;;   Returns:
    ;;     boolean: #t after staging.
    ((self '~path-check) path)
    (let ((standard ((self '~field) 'standard))
          (stage ((self '~field) 'stage)))
      ((standard 'deep-set!) stage path value)
      ((self '~field) 'stage stage)))

  (define-method (get self path pinned? proof?)
    ;; Get value at path, optionally with proof details.
    ;;   Args:
    ;;     path (list): path segments.
    ;;     pinned? (boolean): include pinned detail.
    ;;     proof? (boolean): include proof detail.
    ;;   Returns:
    ;;     any: value or association list with content/pinned?/proof.
    (let* ((standard ((self '~field) 'standard))
           (obj (if (integer? (car path)) ((self '~fetch) path)
                    (let ((stage ((self '~field) 'stage)))
                      ((self '~path-check) path) stage)))
           (content ((standard 'deep-get) obj path)))
      (if (not (or pinned? proof?)) content
          `((content ,content)
            ,@(if (not pinned?) '()
                  (let ((perm ((self '~field) 'perm)))
                    (if (integer? (car path)) ((standard 'deep-prune!) perm path))
                    `((pinned? ,(not (equal? (perm) (((self '~field) 'perm))))))))
            ,@(if (not proof?) '()
                  (begin ((standard 'deep-slice!) obj path)
                         `((proof ,((standard 'serialize) (obj)
                                    `(lambda (node)
                                       (let recurse ((node node))
                                         (if (not (and (sync-node? node) (sync-pair? node))) (sync-cut node)
                                             (sync-cons (recurse (sync-car node)) (recurse (sync-cdr node)))))))))))))))

  (define-method (pin! self path)
    ;; Pin a path into the permanent chain.
    ;;   Args:
    ;;     path (list): path segments.
    ;;   Returns:
    ;;     boolean: #t after pinning.
    ((self '~path-check) (cdr path) #t)
    (let* ((standard ((self '~field) 'standard))
           (perm ((self '~field) 'perm))
           (obj ((self '~fetch) path #t)))
      ((standard 'deep-merge!) obj perm)
      ((self '~field) 'perm perm)))

  (define-method (unpin! self path)
    ;; Remove pinned path from permanent chain.
    ;;   Args:
    ;;     path (list): path segments.
    ;;   Returns:
    ;;     boolean: #t after unpinning.
    ((self '~path-check) (cdr path) #t)
    (let* ((standard ((self '~field) 'standard))
           (perm ((self '~field) 'perm)))
      ((standard 'deep-prune!) perm path)
      ((self '~field) 'perm perm)))

  (define-method (synchronize self index)
    ;; Serialize bridge-visible chain digest at index for sync.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;   Returns:
    ;;     list: serialization list.
    (let ((standard ((self '~field) 'standard))
          (perm ((self '~field) 'perm)))
      ((standard 'serialize) (perm)
       `(lambda (node)
          (let ((chain ((eval (byte-vector->expression (sync-car node))) node)))
            (if (> ((chain 'size)) 0)
                (let* ((node ((chain 'get) -1))
                       (tree ((eval (byte-vector->expression (sync-car node))) node)))
                  ((tree 'get) '(*crypto* public-key))
                  ((tree 'get) '(*crypto* signature))
                  ((chain 'digest) ,index))))))))

  (define-method (resolve self index path)
    ;; Resolve a remote path against a serialized chain at index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;     path (list): path segments.
    ;;   Returns:
    ;;     list: serialization list of the resolved object.
    ((self '~path-check) (cdr path) #t)
    (let ((standard ((self '~field) 'standard))
          (obj ((self '~fetch) path #t index)))
      ((standard 'serialize) (obj)
       `(lambda (node)
          (letrec ((chain ((eval (byte-vector->expression (sync-car node))) node))
                   (deep-get (lambda (object path)
                               (if (null? path) object
                                   (let ((node ((object 'get) (car path))))
                                     (if (not (sync-node? node)) (deep-get node (cdr path))
                                         (let ((object ((eval (byte-vector->expression (sync-car node))) node)))
                                           (deep-get object (cdr path)))))))))
            (deep-get chain ',path))))))
  
  (define-method (step-bridge! self name)
    ;; Fetch and validate a bridge chain head into stage.
    ;;   Args:
    ;;     name (symbol): bridge name.
    ;;   Returns:
    ;;     boolean: #t when bridge state updated, #f if bridge missing.
    (let* ((config ((self '~field) 'config))
           (standard ((self '~field) 'standard))
           (perm ((self '~field) 'perm))
           (stage ((self '~field) 'stage)))
      (if (eq? ((config 'get) `(private bridge ,name)) '()) #f
          (let* ((value ((standard 'deep-get) perm `(-1 (*bridge* ,name chain))))
                 (last (if (procedure? value) value #f))
                 (index (if last (- ((last 'size)) 1) -1))
                 (synchronize (eval ((config 'get) `(private bridge ,name synchronize))))
                 (serialized (synchronize index))
                 (deserialized ((standard 'deserialize) serialized))
                 (latest ((standard 'load) deserialized)))
            (if (= ((latest 'size)) 0)
                ((stage 'set!) `(*bridge* ,name valid?) #t)
                (begin ((self '~signature-verify) latest ((config 'get) `(private bridge ,name public-key)))
                       ((stage 'set!) `(*bridge* ,name valid?)
                        (if (and last (> ((last 'size)) 0))
                            (equal? ((last 'digest)) ((latest 'digest) index)) #t))))
            ((stage 'set!) `(*bridge* ,name chain) (latest))
            ((self '~field) 'stage stage)))))

  (define-method (step-chain! self)
    ;; Commit staged changes to permanent chain and update temp window.
    ;;   Returns:
    ;;     integer: new chain size.
    (let* ((config ((self '~field) 'config))
           (window ((config 'get) '(public window)))
           (standard ((self '~field) 'standard))
           (stage ((self '~field) 'stage))
           (perm ((self '~field) 'perm))
           (temp ((self '~field) 'temp))
           (prev-digest (if (= ((perm 'size)) 0) (sync-digest (sync-null))
                            (let ((prev ((standard 'load) ((perm 'get) -1))))
                              ((prev 'set!) '(*crypto*) '(nothing))
                              (sync-digest (prev))))))
      (if (not (equal? (sync-digest (stage)) prev-digest))
          (begin ((stage 'set!) '(*state* *time*) (system-time-utc))
                 ((perm 'push!) (stage))
                 ((self '~signature-sign!) perm)
                 ((temp 'push!) ((perm 'get) -1))
                 (if (and window (>= ((temp 'size)) window)) ((temp 'prune!) (- window)))
                 ((standard 'deep-prune!) perm '(-1 (*state*)))
                 ((self '~field) 'stage stage)
                 ((self '~field) 'perm perm)
                 ((self '~field) 'temp temp)
                 ((self 'pin!) '(-1 (*state* *time*)))))
      ((perm 'size))))

  (define-method (step-generate self)
    ;; Build a list of step calls (local chain then bridges) for iteration.
    ;;   Returns:
    ;;     list: step expressions.
    (let ((config ((self '~field) 'config)))
      (let loop ((bridges ((config 'get) '(private bridge))) (steps '((step-chain!))))
        (if (null? bridges) (reverse steps)
            (loop (cdr bridges) (cons `(step-bridge! ,(caar bridges)) steps))))))

  (define-method (~fetch self path slice? (index -1))
    ;; Fetch chain node at path, optionally slicing and using remote resolution.
    ;;   Args:
    ;;     path (list): path segments.
    ;;     slice? (boolean): whether to slice proof.
    ;;     index (integer): chain index to use.
    ;;   Returns:
    ;;     chain object: chain object with requested path loaded.
    (let* ((config ((self '~field) 'config))
           (standard ((self '~field) 'standard))
           (perm ((self '~field) 'perm))
           (window ((config 'get) '(public window)))
           (target ((perm 'index) (if (>= (car path) 0) (car path) (+ index 1 (car path)))))
           (current (- ((perm 'size)) 1)))
      (let* ((chain (if (and window (< target (- current window))) perm ((self '~field) 'temp)))
             (chain ((chain 'previous) index)))
        (if (and (eq? (caadr path) '*bridge*) (> (length path) 2))
            (let* ((pref (reverse (list-tail (reverse path) (- (length path) 2))))
                   (name (cadadr pref))
                   (head ((standard 'deep-get) chain pref)))
              (if (= ((head 'size)) 0)
                  (error 'fetch-error "Cannot fetch from remote chain of size 0")
                  (let* ((remote-index ((head 'index) -1))
                         (remote-path (cons ((head 'index) (path 2)) (list-tail path 3)))
                         (serialized ((eval ((config 'get) `(private bridge ,name resolve))) remote-index remote-path))
                         (deserialized ((standard 'deserialize) serialized))
                         (body ((standard 'load) deserialized)))
                    (if (not (equal? (sync-digest (body)) (sync-digest (head))))
                        (error 'digest-error "Remote chain does not match local chain head")
                        ((standard 'deep-call!) chain pref
                         (lambda (head)
                           ((standard 'deep-merge!) body head))))
                    (if slice? ((standard 'deep-slice!) chain path)) chain)))
            (begin
              (if slice? ((standard 'deep-slice!) chain path)) chain)))))

  (define-method (update-window self window)
    ;; Update the configured chain window and prune temp if the window shrinks.
    ;;   Args:
    ;;     window (integer or #f): recent-chain window size, or #f to disable.
    ;;   Returns:
    ;;     boolean: #t after updating config and temp.
    (let* ((config ((self '~field) 'config))
           (temp ((self '~field) 'temp))
           (old-window ((config 'get) '(public window)))
           (size ((temp 'size))))
      ((config 'set!) '(public window) window)
      (if (and (integer? window) (>= size window))
          (let loop ((i (if (integer? old-window) (if (< window old-window) (max 0 (+ (- size old-window) 1)) (+ size 1)) 0)))
            (if (> i (- size window)) #t
                (begin ((temp 'prune!) i)
                       (loop (+ i 1))))))
      ((self '~field) 'temp temp)
      ((self '~field) 'config config)))

  (define-method (update-config! self path value)
    ;; Update a configuration entry in place.
    ;;   Args:
    ;;     path (list): configuration path to update.
    ;;     value (any): replacement value.
    ;;   Returns:
    ;;     boolean: #t after updating config.
    (let ((config ((self '~field) 'config)))
      ((config 'set!) path value)
      ((self '~field) 'config config)))

  (define-method (update-code! self class update!)
    ;; Update one surface-level code object in place.
    ;;   Args:
    ;;     class (symbol): update target (`standard`, `config`, `tree`, or `chain`).
    ;;     update! (procedure): function of form (lambda (obj) ... obj)
    ;;   Returns:
    ;;     boolean: #t after updating code.
    (case class
      ((standard) ((self '~field) 'standard (update! ((self '~field) 'standard))))
      ((config) ((self '~field) 'config (update! ((self '~field) 'config))))
      ((tree) ((self '~field) 'stage (update! ((self '~field) 'stage))))
      ((chain) ((self '~field) 'perm (update! ((self '~field) 'perm)))
       ((self '~field) 'temp (update! ((self '~field) 'temp))))
      (else (error 'case-error "Unrecognized class update candidate"))))))
