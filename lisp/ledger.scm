(define-class (ledger)
  ;; Ledger class manages config, staged state, and signed/pinned chain history.
  (define-method (*init* self standard (config '()) tree-class chain-class)
    ;; Initialize ledger with a standard helper, inline config data, and fresh storage classes.
    ;;   Args:
    ;;     standard (standard object): standard helper instance.
    ;;     config (list): initial config expression.
    ;;   Returns:
    ;;     boolean: #t after setting fields.
    ((self '~field!) 'standard standard)
    ((self '~field!) 'config (expression->byte-vector config))
    (let ((standard-obj (sync-eval standard #f)))
      ((self '~field!) 'stage ((standard-obj 'init) tree-class))
      ((self '~field!) 'temp ((standard-obj 'init) chain-class))
      ((self '~field!) 'perm ((standard-obj 'init) chain-class))))

  (define-method (config self (path '()))
    ;; Return inline ledger config data.
    ;;   Args:
    ;;     path (list): optional nested config path.
    ;;   Returns:
    ;;     list: config expression or subexpression.
    ((self '~config-get) path))

  (define-method (info self)
    ;; Return public config info only.
    ;;   Returns:
    ;;     list: public config expression.
    ((self '~config-get) '(public)))

  (define-method (size self)
    ;; Size of the permanent chain.
    ;;   Returns:
    ;;     integer: chain size.
    (let ((perm (sync-eval ((self '~field!) 'perm) #f)))
      ((perm 'size))))

  (define-method (bridge! self name interface info)
    ;; Apply prepared bridge registration data and cache the bridge public key.
    ;;   Args:
    ;;     info (list): bridge info payload.
    ;;   Returns:
    ;;     boolean: #t after updating config.
    (let ((public-key (cadr (assoc 'public-key info))))
      ((self '~config-set!) `(private bridge ,name) info)
      ((self '~config-set!) `(private bridge ,name interface) interface)
      ((self '~config-set!) `(private bridge ,name public-key) public-key)))

  (define-method (get self path)
    ;; Get value at a stage path
    ;;   Args:
    ;;     path (list): path segments.
    ;;   Returns:
    ;;     any: value or association list with content/pinned?/proof.
    ((self '~path-check) path #t #f)
    (let* ((standard (sync-eval ((self '~field!) 'standard) #f))
           (stage ((self '~field!) 'stage)))
      ((standard 'deep-get) stage path)))

  (define-method (set! self path value)
    ;; Stage a state change at path (not yet committed).
    ;;   Args:
    ;;     path (list): path segments.
    ;;     value (any): value to set.
    ;;   Returns:
    ;;     boolean: #t after staging.
    ((self '~path-check) path #t #t)
    (if (> (length path) 2)
        (let ((standard (sync-eval ((self '~field!) 'standard) #f))
              (stage ((self '~field!) 'stage)))
          ((self '~field!) 'stage ((standard 'deep-set!) stage path value)))
        (let ((stage-obj (sync-eval ((self '~field!) 'stage) #f)))
          ((stage-obj 'set!) (car path) value)
          ((self '~field!) 'stage (stage-obj)))))

  (define-method (set-batch! self paths values)
    (map (lambda (path) ((self '~path-check) path #t #t)) paths)
    (let ((stage-obj (sync-eval ((self '~field!) 'stage) #f)))
      ((stage-obj 'set-batch!) (map car paths) values)
      ((self '~field!) 'stage (stage-obj))))

  (define-method (resolve self path pinned? proof? head)
    ;; Get value at path, optionally with proof details.
    ;;   Args:
    ;;     path (list): path segments.
    ;;     pinned? (boolean): include pinned detail.
    ;;     proof? (boolean): include proof detail.
    ;;   Returns:
    ;;     any: value or association list with content/pinned?/proof.
    ((self '~path-check) path #f #f)
    (let* ((standard (sync-eval ((self '~field!) 'standard) #f))
           (head (if head head ((self '~head) path)))
           (content ((standard 'deep-get) head path)))
      (if (not (or pinned? proof?)) content
          `((content ,content)
            ,@(if (not pinned?) '()
                  (let ((perm ((self '~field!) 'perm)))
                    (if (integer? (car path)) (set! perm ((standard 'deep-prune!) perm path)))
                    `((pinned? ,(not (equal? perm ((self '~field!) 'perm)))))))
            ,@(if (not proof?) '()
                  (let ((proof ((standard 'deep-slice!) head path)))
                    `((proof ,((standard 'serialize) proof)))))))))

  (define-method (pin! self path response)
    ;; Merge a prepared pinned proof object into the permanent chain.
    ;;   Args:
    ;;     response (list): serialized object returned by `pin~`.
    ;;   Returns:
    ;;     boolean: #t after pinning.
    (let* ((standard (sync-eval ((self '~field!) 'standard) #f))
           (head-node (if response ((standard 'deserialize) response) ((standard 'deep-slice!) ((self '~head) path) path)))
           (head-obj (sync-eval head-node #f))
           (head-index (- ((head-obj 'size)) 1)))
      ((self '~field!) 'perm ((standard 'deep-merge!) ((head-obj 'get) -1) ((self '~field!) 'perm) `(,head-index)))))

  (define-method (unpin! self path)
    ;; Remove a path from the permanent chain.
    ;;   Args:
    ;;     path (list): path segments.
    ;;   Returns:
    ;;     boolean: #t after unpinning.
    ((self '~path-check) path #f #f)
    (let* ((standard (sync-eval ((self '~field!) 'standard) #f))
           (perm ((self '~field!) 'perm)))
      ((self '~field!) 'perm ((standard 'deep-prune!) perm path))))

  (define-method (synchronize self index)
    ;; Serialize bridge-visible chain digest at index for sync.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;   Returns:
    ;;     list: serialization list.
    (let ((standard (sync-eval ((self '~field!) 'standard) #f))
          (perm (sync-eval ((self '~field!) 'perm) #f)))
      ((standard 'serialize) (perm)
       `(lambda (node)
          (let ((chain (sync-eval node #f)))
            (if (> ((chain 'size)) 0)
                (let* ((node ((chain 'get) -1))
                       (tree (sync-eval node #f)))
                  ((tree 'get) '(*crypto* public-key))
                  ((tree 'get) '(*crypto* signature))
                  ((chain 'digest) ,index))))))))

  (define-method (trace self index path head)
    ;; Trace a remote path against a serialized chain at index.
    ;;   Args:
    ;;     index (integer): index to access.
    ;;     path (list): path segments.
    ;;   Returns:
    ;;     list: serialization list of the traced object.
    ((self '~path-check) path #f #f)
    (let* ((standard (sync-eval ((self '~field!) 'standard) #f))
           (head (if head head ((self '~head) path index))))
      ((standard 'serialize) head
       `(lambda (node)
          (letrec ((deep-get (lambda (node path)
                               (if (null? path) node
                                   (let ((child (((sync-eval node) 'get) (car path))))
                                     (if (not (sync-node? child)) child
                                         (deep-get child (cdr path))))))))
            (deep-get node ',path))))))
  
  (define-method (bridge-synchronize! self name index response)
    ;; Apply a prepared bridge synchronization payload into staged bridge state.
    ;;   Args:
    ;;     response (list): payload returned by `bridge-synchronize~`.
    ;;   Returns:
    ;;     boolean: #t when bridge state updated, #f if bridge missing.
    (let* ((standard (sync-eval ((self '~field!) 'standard) #f))
           (perm (sync-eval ((self '~field!) 'perm) #f))
           (stage (sync-eval ((self '~field!) 'stage) #f)))
      (if (eq? ((self '~config-get) `(private bridge ,name)) '()) #f
          (let* ((init (> ((perm 'size)) 0))
                 (value (if init ((stage 'get) `(*bridge* ,name chain)) #f))
                 (last (if (sync-node? value) (sync-eval value #f) #f)))
            (if (and last (> index 0) (< index (- ((last 'size)) 1))) #f
                (let* ((deserialized ((standard 'deserialize) response))
                       (latest deserialized)
                       (latest-obj (sync-eval latest #f)))
                  (if (< index 0)
                      ((stage 'set!) `(*bridge* ,name valid?) #t)
                      (begin ((self '~signature-verify) latest ((self '~config-get) `(private bridge ,name public-key)))
                             ((stage 'set!) `(*bridge* ,name valid?)
                              (if (and last (> ((last 'size)) 0))
                                  (equal? ((last 'digest)) ((latest-obj 'digest) index)) #t))))
                  ((stage 'set!) `(*bridge* ,name chain)
                   (if (> ((latest-obj 'size)) 0)
                       ((standard 'deep-slice!) latest '(-1 ()))
                       latest))
                  ((self '~field!) 'stage (stage))))))))

  (define-method (step! self unix-time)
    ;; Commit staged changes to permanent chain and update temp window.
    ;;   Args:
    ;;     unix-time (integer): step time as unix epoch seconds.
    ;;   Returns:
    ;;     integer: new chain size.
    (let* ((window ((self '~config-get) '(public window)))
           (standard (sync-eval ((self '~field!) 'standard) #f))
           (stage (sync-eval ((self '~field!) 'stage) #f))
           (perm (sync-eval ((self '~field!) 'perm) #f))
           (temp (sync-eval ((self '~field!) 'temp) #f))
           (prev-digest (if (= ((perm 'size)) 0) (sync-digest (sync-null))
                            (sync-digest
                             ((standard 'deep-call!) ((perm 'get) -1) '()
                              '(lambda (obj)
                                 ((obj 'set!) '(*crypto*) '(nothing))))))))
      (if (not (equal? (sync-digest (stage)) prev-digest))
          (begin ((stage 'set!) '(*state* *time*) (system-time-utc unix-time))
                 ((perm 'push!) (stage))
                 (set! perm (sync-eval ((self '~signature-sign!) (perm)) #f))
                 ((temp 'push!) ((perm 'get) -1))
                 (let ((time-node ((standard 'deep-slice!) (temp) '(-1 (*state* *time*)))))
                   (if (and window (> ((temp 'size)) window)) ((temp 'prune!) (- (+ window 1))))
                   ((self '~field!) 'stage (stage))
                   ((self '~field!) 'perm ((standard 'deep-merge!) time-node ((standard 'deep-prune!) (perm) '(-1 (*state*)))))
                   ((self '~field!) 'temp (temp)))))
      ((perm 'size))))

  (define-method (update-config! self path value)
    ;; Update a config entry in place.
    ;;   Args:
    ;;     path (list): config path to update.
    ;;     value (any): replacement value.
    ;;   Returns:
    ;;     boolean: #t after updating config.
    (if (equal? path '(public window))
        (let* ((temp (sync-eval ((self '~field!) 'temp) #f))
               (old-window ((self '~config-get) '(public window)))
               (size ((temp 'size))))
          (if (and (integer? value) (> size value))
              (let loop ((i (if (and (integer? old-window) (< value old-window))
                                (max 0 (- size old-window))
                                (+ size 1))))
                (if (> i (- size value 1)) #t
                    (begin ((temp 'prune!) i)
                           (loop (+ i 1))))))
          ((self '~field!) 'temp (temp))))
    ((self '~config-set!) path value))

  (define-method (update-code! self class (update '(lambda (obj) obj)))
    ;; Update one surface-level code object in place.
    ;;   Args:
    ;;     class (symbol): update target (`standard`, `tree`, or `chain`).
    ;;     update! (expr): quoted function of form '(lambda (obj) ... obj)
    ;;   Returns:
    ;;     boolean: #t after updating code.
    (let ((update-func (eval update)))
      (case class
        ((standard) ((self '~field!) 'standard (update-func ((self '~field!) 'standard))))
        ((tree) ((self '~field!) 'stage (update-func ((self '~field!) 'stage))))
        ((chain) ((self '~field!) 'perm (update-func ((self '~field!) 'perm)))
         ((self '~field!) 'temp (update-func ((self '~field!) 'temp))))
        (else (error 'case-error "Unrecognized class update candidate")))))

  (define-method (~field! self name value)
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
                     (else (error 'field-error "Field not found")))))
      (if value (set! (self address) value)
          (self address))))

  (define-method (~path-check self path stage? state-only?)
    ;; Validate a state/bridge path shape before access.
    ;;   Args:
    ;;     path (list): path segments.
    ;;     stage? (boolean): whether this is a staged-state path rather than an indexed chain path.
    ;;     bridge-okay? (boolean): allow *bridge* paths.
    ;;   Returns:
    ;;     boolean: #t when valid (or raises on invalid path).
    (let ((invalid-chain? (lambda (x) (if (or (and (not (null? x)) (not (integer? (car x))))
                                              (and (> (length x) 1) (not (pair? (cadr x))))) #t #f)))
          (invalid-stage? (lambda (x) (if (and (not (null? x)) (not (pair? (car x)))) #t #f)))
          (invalid-state? (lambda (x) (if (or (not (list? x)) (null? x) (null? (car x)) (not (eq? (caar x) '*state*))) #t #f))))
      (cond ((not (list? path))
             (error 'path-error "..."))
            ((and stage? state-only? (or (invalid-stage? path) (invalid-state? path)))
             (error 'path-error "..."))
            ((and stage? (not state-only?) (invalid-stage? path))
             (error 'path-error "..."))
            ((and (not stage?) state-only? (or (invalid-chain? path) (null? path) (invalid-state? (cdr path))))
             (error 'path-error "..."))
            ((and (not stage?) (not state-only?) (invalid-chain? path))
             (error 'path-error "..."))
            (else #t))))

  (define-method (~config-get self (path '()))
    (let loop ((config (byte-vector->expression ((self '~field!) 'config))) (path path))
      (if (null? path) config
          (let ((match (assoc (car path) config)))
            (if (not match) '()
                (loop (cadr match) (cdr path)))))))

  (define-method (~config-set! self (path '()) value)
    ((self '~field!) 'config
     (expression->byte-vector
      (let loop-1 ((config (byte-vector->expression ((self '~field!) 'config))) (path path))
        (if (null? path) value
            (let loop-2 ((config config))
              (cond ((null? config)
                     (if (eq? value '()) '()
                         (list (list (car path) (loop-1 '() (cdr path))))))
                    ((eq? (caar config) (car path))
                     (let ((result (loop-1 (cadar config) (cdr path))))
                       (if (eq? result '()) (cdr config)
                           (cons (list (car path) result) (cdr config)))))
                    (else (cons (car config) (loop-2 (cdr config)))))))))))

  (define-method (~head self path (index -1))
    ;; Fetch the appropriate local chain head node for a path/index.
    ;;   Args:
    ;;     path (list): path segments.
    ;;     index (integer): chain index to use.
    ;;   Returns:
    ;;     sync node: chain head node.
    (let* ((standard (sync-eval ((self '~field!) 'standard) #f))
           (perm (sync-eval ((self '~field!) 'perm) #f))
           (window ((self '~config-get) '(public window)))
           (target ((perm 'index) (cond ((null? path) -1) ((>= (car path) 0) (car path)) (else (+ index 1 (car path))))))
           (current (- ((perm 'size)) 1)))
      (let ((chain (if (and window (<= target (- current window))) perm (sync-eval ((self '~field!) 'temp) #f))))
        ((chain 'previous) index))))

  (define-method (~signature-sign! self chain)
    ;; Embed public key and signature into chain head using config keys.
    ;;   Args:
    ;;     chain (sync node): chain node to sign.
    ;;   Returns:
    ;;     sync node: signed chain node.
    (let* ((standard (sync-eval ((self '~field!) 'standard) #f))
           (public-key ((self '~config-get) '(public public-key)))
           (secret-key ((self '~config-get) '(private secret-key))))
      (set! chain ((standard 'deep-set!) chain '(-1 (*crypto* public-key)) #u()))
      (set! chain ((standard 'deep-set!) chain '(-1 (*crypto* signature)) #u()))
      ((standard 'deep-call!) chain '(-1)
       `(lambda (tree)
          ((tree 'set!) '(*crypto* public-key) ,public-key)
          ((tree 'set!) '(*crypto* signature) (crypto-sign ,secret-key ,(sync-digest chain)))))))

  (define-method (~signature-verify self chain public-key)
    ;; Verify chain head signature and optional expected public key.
    ;;   Args:
    ;;     chain (sync node): chain node to verify.
    ;;     public-key (byte-vector or #f): expected public key.
    ;;   Returns:
    ;;     boolean: #t when signature verifies (or raises on failure).
    (let* ((standard (sync-eval ((self '~field!) 'standard) #f))
           (chain-copy chain))
      (set! chain-copy ((standard 'deep-set!) chain-copy '(-1 (*crypto* public-key)) #u()))
      (set! chain-copy ((standard 'deep-set!) chain-copy '(-1 (*crypto* signature)) #u()))
      (let* ((included-key ((standard 'deep-call) chain '(-1)
                            '(lambda (tree)
                               ((tree 'get) '(*crypto* public-key)))))
             (signature ((standard 'deep-call) chain '(-1)
                         '(lambda (tree)
                            ((tree 'get) '(*crypto* signature))))))
        (cond ((or (null? signature) (null? included-key))
               (error 'signature-error "Chain doesn not include necessary public key and signature"))
              ((and public-key (not (equal? public-key included-key)))
               (error 'signature-error "Included key does not match expected public key"))
              ((not (crypto-verify (if public-key public-key included-key) signature (sync-digest chain-copy)))
               (error 'signature-error "Included signature does not verify"))
              (else #t))))))
