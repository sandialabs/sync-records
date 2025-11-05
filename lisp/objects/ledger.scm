;; changes needed
;; - strip out the serial logic and use log chain
;; - get rid of message signing
;; - add block signing
;; - instead of prune/slicing the root used dedicated records

;; combine "all" with "get" and introduce "directory?"
;; get/set
;; - nothing/value
;; - value/directory/nothing/unknown
;; get/all/set/delete
;; so basically revert and just combine "object" with "expression" into "content"

;; architecture
;; - how many trees?
;;   - private config 
;;   - staging record
;;   - state chain
;;   - history chain

;; question: how to handle top-level functionality
;; - develop modules
;; - bundle into class
;;   - initialize chain/record separately and pass in composition-style
;;   - will force me to organize better
;; - keep it the way it is

;; question: is it necessary to separate get/set? or should it just be one "call"?
;; - I think so... otherwise it wouldn't be clear how to set things on the way back up
;;   - wait, but doesn't the call itself take care of the behavior?

;; together
;; - ((get path1) (get path2) (get path3))
;; - ((set! path1) (set! path2) (set! path3 value))
;; - ((get path1) (set! path2) value)
;; - ((set! path1) (set! path2) (truncate! blah blah

;; options
;; - single combined thing (ideal, but might be verbose)
;; - somehow only have get and set
;; - what if we get and set?
;;   - pass on the way down, both return and state on the way up, set if necessary
;;   - so query, essentially
;; - have some version for each of them (copy, truncate, etc)

(macro (secret blocking? window lifetime)
  "> blocking? (bool): if #t, then all network connections block until success or timeout
  > window (uint): number of indices (steps) to keep old state data or #f if keep forever
  < return (fnc): configured ledger setup function"

  `(lambda (root)
     "Extend the root interface to include ledger functionality. The
     ledger extension provides logic for version-controlling stateful.
     Optionally, ledgers be configured to delete older states while
     persisting explicitly 'pinned' exceptions. Finally, ledgers support
     peering, syncing, and reading historical state from other ledgers
     across transitive peer-to-peer connections.

     > root (fnc): library to access root commands
     < return (str): success boolean"

     ;; --- Helper Functions ---

     (define signature-sign
       '(lambda (message)
          "Accept a message and return an assertion containing the ledger's public key and signature"
          (let* ((config (cadr ((root 'get) '(ledger meta config))))
                 (public-key (cadr (assoc 'public-key config)))
                 (secret-key (cadr (assoc 'secret-key config)))
                 (signature (crypto-sign secret-key (expression->byte-vector message))))
            (append message `(((public-key ,public-key) (signature ,signature)))))))

     (define signature-verify
       '(lambda (message assertion)
          "Verify the signature of a message given the message and assertion containing a public key"
          (let* ((public-key (cadr (assoc 'public-key assertion)))
                 (signature (cadr (assoc 'signature assertion))))
            (crypto-verify public-key signature (expression->byte-vector message)))))

     (define load-object
       '(lambda (node)
          ((eval (byte-vector->expression (sync-car node))) node)))

     (define call-peer
       `(lambda (name)
          (sync-call 
           `(*root* ;; todo: this really should use the local interface
             ,,,secret
             (lambda (root)
               (let* ((config (cadr ((root 'get) '(ledger meta peers ,name))))
                      (messenger (eval (cadr (assoc 'messenger config))))
                      (message (,',signature-sign '(ledger-synchronize)))
                      (result (messenger message))
                      (stage `(,load-object (cadr ((root 'get) '(ledger stage))))))
                 (root 'set!) '(ledger stage) ((stage 'set!) `(*peers* ,name) result))))
           ,,blocking?)))

     (define call-step
       `(lambda (root)
          (let* ((config (cadr ((root 'get) '(ledger meta config))))
                 (window (cadr (assoc 'window config)))
                 (lifetime (cadr (assoc 'lifetime config)))
                 (chain-1 (,load-object (cadr ((root 'get) '(ledger chain-1)))))
                 (chain-2 (,load-object (cadr ((root 'get) '(ledger chain-2)))))
                 (stage (,load-object (cadr ((root 'get) '(ledger stage)))))
                 (index (+ (cadr ((root 'get) '(ledger index))) 1)))
            ((chain-1 'push!) (stage))
            ((stage 'prune!) '() '(*state*))
            ((chain-2 'push!) (stage))
            (if (= (modulo index (expt 2 window)) 0) ((chain-1 'truncate!) window))
            (if (= (modulo index (expt 2 lifetime)) 0) ((chain-2 'truncate!) lifetime))
            ((root 'set!) '(ledger chain-1) `(content ,(chain-1)))
            ((root 'set!) '(ledger chain-2) `(content ,(chain-2)))
            ((root 'set!) '(ledger index) `(content ,index))
            index)))

     (define ledger-config-local
       '(lambda (root)
          (cadr ((root 'get) '(ledger meta config)))))

     (define ledger-config-remote
       '(lambda (root)
          (let ((config (cadr ((root 'get) '(ledger meta config)))))
            `(,(assoc 'public-key config)))))

     (define ledger-index
       `(lambda (root)
          "Return the current index (i.e., the lowest index that has not been finalized)

          > root (fnc): library to access root commands
          < return (uint): the current index"
          ((root 'get) '(ledger index))))

     (define peer-prove
       `(lambda (root name chain-path remote-path)
          (let ((config ((root 'get) `(ledger meta peers ,name))))
            (if (eq? (car config) 'nothing) (error 'peer-error "Peer not found")
                (let ((messenger (eval (cadr (assoc 'messenger (cadr config)))))
                      (index (cadr ((root 'get) (append chain-path '(index)))))
                      (i-remote (cadr ((root 'get) (append chain-path `(*peers* ,name index))))))
                  (messenger (,signature-sign `(ledger-prove ,remote-path ,i-remote))))))))

     ;; --- API Functions ---

     (define ledger-step!
       `(lambda (root)
          "Synchronize with all active peers and increment the state"
          (let ((result ((root 'get) '(ledger meta peers))))
            (let loop ((names (if (eq? (car result) 'directory) (cadr result) '())))
              (if (null? names) 'done
                  (begin (,call-peer (car names))
                         (loop (cdr names))))))
          (sync-call '(*call* ,,secret ,call-step) #t)))


     (define ledger-fetch
       `(lambda (root path index store)
          ;; (let* ((chain-path (,ledger-path root index))
          ;;        (index (cadr ((root 'get) (append chain-path '(index))))))
          ;;   (if (not (and (eq? (car path) '*peers*) (> (length path) 1)))
          ;;       (error 'path-error "Invalid query path")
          ;;       (let ((result (,peer-prove root (cadr path) chain-path (list-tail path 2)))
          ;;             (path-peer (append chain-path `(*peers* ,(cadr path)))))
          ;;         ((root 'deserialize!) '(control scratch fetch) result)
          ;;         (if (not ((root 'equivalent?) path-peer '(control scratch fetch)))
          ;;             (error 'integrity-error "Data does not verify"))
          ;;         ((root 'copy!) '(control scratch fetch) store))))))
          #t))

     (define ledger-pin!
       `(lambda (root path index)
          "Pin a historical state to prevent automatic deletion or remote state for caching

          > root (fnc): library to access root commands
          > path (list sym|vec): path to the data to pin
          > index (int): step number of the data to pin
          < return (bool): boolean indicating success of the operation"
          (let* ((chain-1 (,load-object (cadr ((root 'get) '(ledger chain-1)))))
                 (chain-2 (,load-object (cadr ((root 'get) '(ledger chain-2)))))
                 (record-1 (,load-object ((chain-1 'get) index)))
                 (record-2 (,load-object ((chain-2 'get) index))))
            ((record-1 'slice!) '() path)
            ((record-2 'merge!) record-1)
            ((chain-2 'set!) index (record-2))
            ((root 'set!) '(ledger chain-2) `(content ,(chain-2))))))

     (define ledger-unpin!
       `(lambda (root path index)
          "Unpin previously pinned data

          > root (fnc): library to access root commands
          > path (list sym|vec): path to the content to unpin
          > index (int): step number of the content to unpin
          < return (bool): boolean indicating success of the operation"
          (let* ((chain-2 (,load-object (cadr ((root 'get) '(ledger chain-2)))))
                 ((record-2 (,load-object ((chain-2 'get) index)))))
            ((chain-2 'set!) index ((record-2 'prune!) path))
            ((root 'set!) '(ledger chain-2) `(content ,(chain-2))))))

     (define ledger-record
       `(lambda (path index)
          (cond ((or (null? path) (not (eq? (car path) '*state*)))
                 (error 'path-error "Path must start with *state* or *peer*"))
                ((not index) (,load-object (cadr ((root 'get) '(ledger stage)))))
                (else (let* ((config (cadr ((root 'get) '(ledger meta config))))
                             (window (cadr (assoc 'window config)))
                             (lifetime (cadr (assoc 'lifetime config)))
                             (current (cadr ((root 'get) '(ledger index)))))
                        (cond ((> index current) (error 'index-error "Index is greater than current"))
                              ((> index (- current window))
                               (let ((chain (,load-object (cadr ((root 'get) '(ledger chain-1))))))
                                 (,load-object ((chain 'get) index))))
                              ((> index (- current lifespan))
                               (let ((chain (,load-object (cadr ((root 'get) '(ledger chain-2))))))
                                 (,load-object ((chain 'get) index))))
                              (else (error 'index-error "Index is less than lifespan of ledger"))))))))

     ;; (let* ((chain-path (,ledger-path root index))
     ;;        (index (cadr (operate (append chain-path '(index)))))
     ;;        (pinned (operate (append `(ledger pinned ,index) path)))
     ;;       (if (or (eq? (car pinned) 'object) (and (eq? (car pinned) 'directory) (caddr pinned))) pinned
     ;;           (cond ((null? path) '(directory (*state* *peers*) #t))
     ;;                 ((eq? (car pinned) 'object) pinned)
     ;;                 ((and (eq? (car path) '*peers*) (null? (cdr path)))
     ;;                  (operate (append chain-path '(*peers*))))
     ;;                 ((and (eq? (car path) '*state*))
     ;;                  (operate (append `(ledger states ,index) (cdr path))))
     ;;                 ((and (eq? (car path) '*peers*))
     ;;                  (let ((store '(control scratch get)))
     ;;                    (,ledger-fetch root path index store)
     ;;                    (operate (append store (list-tail path 2)))))
     ;;                 (else (error 'path-error "Path must start with *state* or *peer*"))))))))

     (define ledger-get
       `(lambda*
         (root path index)
         "Retrieve the data at the given path and index, potentially from other peers

          > root (fnc): library to access root commands
          > path (list sym|vec): path to the content to retrieve
          > index (int): step number of the content to retrieve
          < return (sym . (list exp)): list containing the type and value of the data
              - 'object type indicates a simple lisp-serializable value
              - 'structure type indicates a complex value represented by sync-pair?
              - 'directory type indicates an intermediate directory node
                - the second item is a list of known subpath segments
                - the third item is a bool indicating whether the directory is complete
                  (i.e., none of its underlying data has been pruned)
              - 'nothing type indicates that no data is found at the path
              - 'unknown type indicates the path has been pruned"
         (let ((record (,ledger-record path index)))
           ((record 'get) path))))

     (define ledger-set!
       `(lambda (root path value)
          "Write the value to the path. Recursively generate parent
          directories if necessary.

          > root (fnc): library to access root commands
          > path (list sym|vec): path to the specified value
          > value (exp|sync-pair): data to be stored at the path
          < return (bool): boolean indicating success of the operation"
          (if (or (null? path) (not (eq? (car path) '*state*)))
              (error 'path-error "first path segment must be *state*")
              (let ((stage (,load-object (cadr ((root 'get) '(ledger stage))))))
                ((stage 'set!) path value)
                ((root 'set!) '(ledger stage) `(content ,(stage)))))))

     (define ledger-copy!
       `(lambda*
         (root path target index)
         "Copy the value from the specified path. Recursively generate parent
          directories if necessary.

          > root (fnc): library to access root commands
          > path (list sym|vec): path to the source location
          > target (list sym|vec): path to the target location
          < return (bool): boolean indicating success of the operation"
         (if (or (null? target) (not (eq? (car target) '*state*)))
             (error 'path-error "first path segment must be *state*")
             (let* ((stage (,load-object (cadr ((root 'get) '(ledger stage)))))
                    (record (,ledger-record target index))
                    (result ((record 'get) path)))
               (case (car result)
                 ((content nothing)
                  ((stage 'set!) target result)
                  ((root 'set!) '(ledger stage) `(content ,(stage))))
                 ((directory) (error 'copy-error "Cannot copy directory type"))
                 ((unknown) (error 'copy-error "Cannot copy unknown type"))
                 (else (error 'copy-error "Cannot copy unrecognized type")))))))

     (define ledger-peer!
       '(lambda (root name messenger)
          "Establish a persistent connection with another peer

          > root (fnc): library to access root commands
          > name (sym): unique symbol to refer to the peer
          > messenger (fnc): function to message (single arg) to send to the peer
          < return (bool): boolean indicating success of the operation"
          ((root 'set!) `(ledger stage *peers* ,name) '(nothing))
          (if (not messenger) ((root 'set!) `(ledger meta peers ,name) '(nothing))
              ((root 'set!) `(ledger meta peers ,name)
               `(content ((messenger ,messenger)
                          ,(assoc 'public-key ((eval messenger) '(ledger-config)))))))))

     (define ledger-peers
       '(lambda (root)
          "List all currently active peers

          > root (fnc): library to access root commands
          < return (list sym): list of peer names"
          (let ((names (cadr ((root 'get) '(ledger meta peers)))))
            (let loop ((names names) (result '()))
              (if (null? names) (reverse result)
                  (let ((info (cadr ((root 'get) `(ledger meta peers ,(car names))))))
                    (loop (cdr names) (cons (cons (car names) info) result))))))))

     (define ledger-prove
       `(lambda (root path index assertion)
          ;; (if (not (,signature-verify `(ledger-prove ,path ,index) assertion))
          ;;     (error 'peer-error "Could not verify assertion")
          ;;     (let* ((chain-path (,ledger-path root index))
          ;;            (index (cadr ((root 'get) (append chain-path '(index)))))
          ;;            (pinned ((root 'get) (append `(ledger pinned ,index) path))))
          ;;       ((root 'copy!) chain-path '(control scratch local))
          ;;       (if (or (eq? (car pinned) 'object) (and (eq? (car pinned) 'directory) (caddr pinned)))
          ;;           ((root 'merge!) `(ledger pinned ,index) '(control scratch local))
          ;;           (begin
          ;;             ((root 'merge!) `(ledger states ,index) '(control scratch local *state*))
          ;;             (if (and (> (length path) 1) (eq? (car path) '*peers*))
          ;;                 (let* ((path-remote (list-tail path 2))
          ;;                        (result (,peer-prove root (cadr path) chain-path path-remote))
          ;;                        (path-root `(control scratch local *peers* ,(cadr path))))
          ;;                   ((root 'deserialize!) '(control scratch remote) result)
          ;;                   (if (not ((root 'equivalent?) '(control scratch remote) path-root))
          ;;                       (error 'peer-error "Ledger integrity error")
          ;;                       ((root 'merge!) '(control scratch remote) path-root))))
          ;;             ((root 'slice!) '(control scratch local) path)
          ;;             (let* ((path-abs (append '(control scratch local) path))
          ;;                    (value ((root 'get) path-abs)))
          ;;               (if (eq? (car value) 'directory)
          ;;                   (let loop ((names (cadr value)))
          ;;                     (if (null? names) #t
          ;;                         (begin
          ;;                           ((root 'prune!) path-abs `(,(car names)) #t)
          ;;                           (loop (cdr names)))))))))
          ;;       ((root 'serialize) '(control scratch local))))))
          #t))

     (define ledger-synchronize
       `(lambda (root assertion)
          ;; (if (not (,signature-verify `(ledger-synchronize) assertion))
          ;;     (error 'peer-error "Could not verify assertion")
          ;;     (let ((chain-path (,ledger-path root -1)))
          ;;       ((root 'copy!) chain-path '(control scratch))
          ;;       ((root 'slice!) '(control scratch) '(index))
          ;;       ((root 'serialize) '(control scratch))))))
          #t))

     (define ledger-library
       `(lambda (root)
          (lambda (function)
            (case function
              ((get) (lambda args (apply ,ledger-get (cons root args))))
              ((set!) (lambda args (apply ,ledger-set! (cons root args))))
              ((copy!) (lambda args (apply ,ledger-copy! (cons root args))))
              ((index) (lambda args (apply ,ledger-index (cons root args))))
              ((peers) (lambda args (apply ,ledger-peers (cons root args))))
              (else (error 'missing-function "Function not found"))))))

     (let* ((key-pair (crypto-generate (expression->byte-vector ,secret)))
            (public-key (car key-pair))
            (secret-key (cdr key-pair)))
       ((root 'set!) '(ledger meta config)
        `(content ((window ,,window)
                   (lifetime ,,lifetime)
                   (public-key ,public-key)
                   (secret-key ,secret-key)))))

     (let* ((std-node (cadr ((root 'get) '(control library standard))))
            (std ((eval (byte-vector->expression (sync-car std-node))) std-node))
            (record-cls (cadr ((root 'get) '(control library record))))
            (chain-cls (cadr ((root 'get) '(control library log-chain)))))
       ((root 'set!) '(ledger stage) `(content ,(((std 'make) record-cls))))
       ((root 'set!) '(ledger chain-1) `(content ,(((std 'make) chain-cls))))
       ((root 'set!) '(ledger chain-2) `(content ,(((std 'make) chain-cls))))
       ((root 'set!) '(ledger index) '(content 0)))

     ((root 'set!) '(control step 0) `(content ,ledger-step!))
     ((root 'set!) '(control local ledger-config) `(content ,ledger-config-local))
     ((root 'set!) '(control local ledger-get) `(content ,ledger-get))
     ((root 'set!) '(control local ledger-set!) `(content ,ledger-set!))
     ((root 'set!) '(control local ledger-copy!) `(content ,ledger-copy!))
     ((root 'set!) '(control local ledger-pin!) `(content ,ledger-pin!))
     ((root 'set!) '(control local ledger-unpin!) `(content ,ledger-unpin!))
     ((root 'set!) '(control local ledger-index) `(content ,ledger-index))
     ((root 'set!) '(control local ledger-peer!) `(content ,ledger-peer!))
     ((root 'set!) '(control local ledger-peers) `(content ,ledger-peers))
     ((root 'set!) '(control remote ledger-config) `(content ,ledger-config-remote))
     ((root 'set!) '(control remote ledger-prove) `(content ,ledger-prove))
     ((root 'set!) '(control remote ledger-synchronize) `(content ,ledger-synchronize))

     ((root 'set!) '(root library ledger) `(content ,ledger-library))))
