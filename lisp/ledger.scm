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

  (define-method (~path-check self path peer-okay?)
    ;; Validate a state/peer path shape before access.
    ;;   Args:
    ;;     path (list): path segments.
    ;;     peer-okay? (boolean): allow *peer* paths.
    ;;   Returns:
    ;;     boolean: #t when valid (or raises on invalid path).
    (cond ((not (list? (car path)))
           (error 'path-error "Expected a list in the next path element"))
          ((and (not peer-okay?) (eq? (caar path) '*peer*))
           (error 'path-error "Expected the first element in the next path element to be '*state*"))
          ((not (or (eq? (caar path) '*state*) (eq? (caar path) '*peer*)))
           (error 'path-error "Expected the first element in the next path element to be '*state* or '*peer*"))
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

  (define-method (*init* self standard config)
    ;; Initialize ledger with a standard API and configuration object.
    ;;   Args:
    ;;     standard (standard class object): standard helper instance.
    ;;     config (configuration object): configuration expression.
    ;;   Returns:
    ;;     boolean: #t after setting fields.
    (let ((tree-class ((config 'get) '(private tree-class)))
          (chain-class ((config 'get) '(private chain-class))))
      ((self '~field) 'standard standard)
      ((self '~field) 'config config)
      ((self '~field) 'stage ((standard 'make) tree-class))
      ((self '~field) 'temp ((standard 'make) chain-class))
      ((self '~field) 'perm ((standard 'make) chain-class))))

  (define-method (configuration self)
    ;; Return full configuration data (private and public).
    ;;   Returns:
    ;;     list: configuration expression.
    (((self '~field) 'config) 'get) '())

  (define-method (information self)
    ;; Return public configuration information only.
    ;;   Returns:
    ;;     list: public configuration expression.
    ((((self '~field) 'config) 'get) '(public)))

  (define-method (size self)
    ;; Size of the permanent chain.
    ;;   Returns:
    ;;     integer: chain size.
    (let ((perm ((self '~field) 'perm)))
      ((perm 'size))))

  (define-method (peer! self name info)
    ;; Register peer info and cache their public key.
    ;;   Args:
    ;;     name (symbol): peer name.
    ;;     info (alist or expression with `information`): peer info.
    ;;   Returns:
    ;;     boolean: #t after updating config.
    ;; todo: reach out and ask peer for public key (maybe? need a "config" endpoint)
    (let ((config ((self '~field) 'config)))
      ((config 'set!) `(private peer ,name) info)
      ((config 'set!) `(private peer ,name public-key)
       (cadr (assoc 'public-key ((eval (cadr (assoc 'information info)))))))
      ((self '~field) 'config config)))

  (define-method (peers self)
    ;; List known peer names.
    ;;   Returns:
    ;;     list: peer names.
    (let ((config ((self '~field) 'config)))
      (map car ((config 'get) '(private peer)))))

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

  (define-method (get self path details?)
    ;; Get value at path, optionally with proof details.
    ;;   Args:
    ;;     path (list): path segments.
    ;;     details? (boolean): include proof details.
    ;;   Returns:
    ;;     any: value or association list with content/pinned?/proof.
    (let* ((standard ((self '~field) 'standard))
           (obj (if (integer? (car path)) ((self '~fetch) path)
                       (let ((stage ((self '~field) 'stage)))
                         ((self '~path-check) path) stage)))
           (content ((standard 'deep-get) obj path)))
      (if (not details?) content
          (let ((perm ((self '~field) 'perm)))
            (if (integer? (car path)) ((standard 'deep-prune!) perm path))
            ((standard 'deep-slice!) obj path)
            `((content ,content)
              (pinned? ,(not (equal? (perm) (((self '~field) 'perm)))))
              (proof ,((standard 'serialize) (obj)
                       `(lambda (node)
                          (let recurse ((node node))
                            (if (not (and (sync-node? node) (sync-pair? node))) (sync-cut node)
                                (sync-cons (recurse (sync-car node)) (recurse (sync-cdr node)))))))))))))

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
    ;; Serialize peer-visible chain digest at index for sync.
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
  
  (define-method (step-peer! self name)
    ;; Fetch and validate a peer chain head into stage.
    ;;   Args:
    ;;     name (symbol): peer name.
    ;;   Returns:
    ;;     boolean: #t when peer state updated, #f if peer missing.
    (let* ((config ((self '~field) 'config))
           (standard ((self '~field) 'standard))
           (perm ((self '~field) 'perm))
           (stage ((self '~field) 'stage)))
      (if (eq? ((config 'get) `(private peer ,name)) '()) #f
          (let* ((value ((standard 'deep-get) perm `(-1 (*peer* ,name chain))))
                 (last (if (procedure? value) value #f))
                 (index (if last (- ((last 'size)) 1) -1))
                 (synchronize (eval ((config 'get) `(private peer ,name synchronize))))
                 (serialized (synchronize index))
                 (deserialized ((standard 'deserialize) serialized))
                 (latest ((standard 'load) deserialized)))
            (if (= ((latest 'size)) 0)
                ((stage 'set!) `(*peer* ,name valid?) #t)
                (begin ((self '~signature-verify) latest ((config 'get) `(private peer ,name public-key)))
                       ((stage 'set!) `(*peer* ,name valid?)
                        (if (and last (> ((last 'size)) 0))
                            (equal? ((last 'digest)) ((latest 'digest) index)) #t))))
            ((stage 'set!) `(*peer* ,name chain) (latest))
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
           (temp ((self '~field) 'temp)))
      ((stage 'set!) '(*state* *time*) (system-time-utc))
      ((perm 'push!) (stage))
      ((self '~signature-sign!) perm)
      ((temp 'push!) ((perm 'get) -1))
      (if (and window (>= ((temp 'size)) window)) ((temp 'prune!) (- window)))
      ((standard 'deep-prune!) perm '(-1 (*state*)))
      ((self '~field) 'stage stage)
      ((self '~field) 'perm perm)
      ((self '~field) 'temp temp)
      ((self 'pin!) '(-1 (*state* *time*)))
      ((perm 'size))))

  (define-method (step-generate self)
    ;; Build a list of step calls (local chain then peers) for iteration.
    ;;   Returns:
    ;;     list: step expressions.
    (let ((config ((self '~field) 'config)))
      (let loop ((peers ((config 'get) '(private peer))) (steps '((step-chain!))))
        (if (null? peers) (reverse steps)
            (loop (cdr peers) (cons `(step-peer! ,(caar peers)) steps))))))

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
        (if (and (eq? (caadr path) '*peer*) (> (length path) 2))
            (let* ((pref (reverse (list-tail (reverse path) (- (length path) 2))))
                   (name (cadadr pref))
                   (head ((standard 'deep-get) chain pref)))
              (if (= ((head 'size)) 0)
                  (error 'fetch-error "Cannot fetch from remote chain of size 0")
                  (let* ((remote-index ((head 'index) -1))
                         (remote-path (cons ((head 'index) (path 2)) (list-tail path 3)))
                         (serialized ((eval ((config 'get) `(private peer ,name resolve))) remote-index remote-path))
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

  (define-method (*update* self class function)
    ;; Update the class logic and return a new ledger object 
    ;;   Args:
    ;;     class (symbol): path segments.
    ;;     function (procedure): function of form (lambda (obj) ... obj)
    ;;   Returns:
    ;;     ledger object: updated ledger object (or raises a case error)
    (let ((clone ((eval (byte-vector->expression (self '(0)))) (self '()))))
      (if (eq? class 'ledger) (function clone)
          (case class
            ((config) ((clone '~field) 'config (function ((clone '~field) 'config))))
            ((tree) ((clone '~field) 'stage (function ((clone '~field) 'stage))))
            ((chain) ((clone '~field) 'perm (function ((clone '~field) 'perm)))
             ((clone '~field) 'perm (function ((clone '~field) 'perm))))
            (else (error 'case-error "Unrecognized class update candidate"))))
      clone)))
