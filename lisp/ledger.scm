(lambda (cryptography blocking? window)
  `(lambda (record secret) 

     (define signature-sign
       '(lambda (message)
          "Accept a message and return an assertion containing the ledger's public key and signature"
          (let* ((config (cadr ((record 'get) '(ledger meta config))))
                 (crypto (cadr (assoc 'cryptography config)))
                 (public-key (cadr (assoc 'public-key config)))
                 (secret-key (cadr (assoc 'secret-key config)))
                 (sec-hex (byte-vector->hex-string secret-key))
                 (hash-hex (byte-vector->hex-string (sync-hash (expression->byte-vector message))))
                 (result (sync-http 'get (append crypto "/signature/sign/" sec-hex "/" hash-hex)))
                 (signature (hex-string->byte-vector (byte-vector->expression result))))
            (append message `(((public-key ,public-key) (signature ,signature)))))))

     (define signature-verify
       '(lambda (message assertion)
          "Verify the signature of a message given the message and assertion containing a public key"
          (let* ((config (cadr ((record 'get) '(ledger meta config))))
                 (crypto (cadr (assoc 'cryptography config)))
                 (pub-hex (byte-vector->hex-string (cadr (assoc 'public-key assertion))))
                 (sig-hex (byte-vector->hex-string (cadr (assoc 'signature assertion))))
                 (hash-hex (byte-vector->hex-string (sync-hash (expression->byte-vector message)))))
            (sync-http 'get (append crypto "/signature/verify/" pub-hex "/" sig-hex "/" hash-hex)))))

     (define call-peer
       `(lambda (name)
          (sync-call 
           `(*record*
             ,,secret
             (lambda (record)
               (let* ((config (cadr ((record 'get) '(ledger meta peers ,name))))
                      (messenger (eval (cadr (assoc 'messenger config))))
                      (message (,',signature-sign '(ledger-synchronize)))
                      (result (messenger message)))
                 ((record 'deserialize!) '(ledger stage *peers* ,name) result))))
           ,,blocking?)))

     (define call-step
       `(lambda (record)
          (let* ((config (cadr ((record 'get) '(ledger meta config))))
                 (window (cadr (assoc 'window config)))
                 (index (cadr ((record 'get) '(ledger chain index)))))
            (let loop ((periodicity 0))
              (let ((period (expt 2 periodicity)))
                (if (or (< (- (+ index 1) period) 0) (not (= (modulo index period) 0))) #t
                    (begin ((record 'copy!) '(ledger chain)
                            (append '(ledger previous) (make-list periodicity 'rest) '(first)))
                           (loop (+ periodicity 1))))))
            (if window ((record 'set!) `(ledger states ,(- (+ index 1) window)) #f))
            ((record 'copy!) '(ledger stage) '(control scratch))
            ((record 'copy!) '(ledger stage *state*) `(ledger states ,(+ index 1)))
            ((record 'copy!) '(ledger previous) '(control scratch previous))
            ((record 'set!) '(control scratch index) (+ index 1))
            ((record 'prune!) '(control scratch) '(*state*) #t)
            ((record 'align!) '(control scratch))
            ((record 'copy!) '(control scratch) '(ledger chain))
            (+ index 1))))

     (define step!
       `(lambda (record)
          "Synchronize with all active peers and increment the state"
          (let loop ((names (cadr ((record 'get) '(ledger meta peers)))))
            (if (null? names) 'done
                (begin (,call-peer (car names))
                       (loop (cdr names)))))
          (sync-call '(*record* ,secret ,call-step) #t)))

     (define ledger-config-local
       '(lambda (record)
          (cadr ((record 'get) '(ledger meta config)))))

     (define ledger-config-remote
       '(lambda (record)
          (let ((config (cadr ((record 'get) '(ledger meta config)))))
            `(,(assoc 'public-key config)))))

     (define ledger-path
       `(lambda*
         (record index)
         (let* ((len (+ (cadr ((record 'get) '(ledger chain index))) 1))
                (index (cond ((not index) (- len 1))
                             ((not (integer? index))
                              (error 'invalid-index "Index must be an integer"))
                             ((and (>= index 0) (< index len)) index)
                             ((>= index 0) 
                              (error 'invalid-index "Index cannot exceed chain length"))
                             ((and (< index 0) (>= (+ len index) 0)) (+ len index))
                             ((< index 0)
                              (error 'invalid-index "Index cannot exceed chain length"))
                             (else (error 'logic-error "Unhandled case")))))
           (let loop-1 ((i (cadr ((record 'get) '(ledger chain index)))) (prev '()))
             (if (> i index)
                 (let loop-2 ((periodicity 0) (prev (append prev '(previous))))
                   (let ((period (expt 2 periodicity)))
                     (if (and (>= (- i (* period 2)) index)
                              (= (modulo i (* period 2)) 0)
                              (<= period i))
                         (loop-2 (+ periodicity 1) (append prev '(rest)))
                         (loop-1 (- i period) (append prev '(first))))))
                 (append '(ledger chain) prev))))))

     (define ledger-index
       `(lambda (record)
          (let ((chain-path (,ledger-path record -1)))
            (cadr ((record 'get) (append chain-path '(index)))))))

     (define peer-prove
       `(lambda (record name chain-path remote-path)
          (let* ((config (cadr ((record 'get) `(ledger meta peers ,name))))
                 (public-key (eval (cadr (assoc 'public-key config))))
                 (messenger (eval (cadr (assoc 'messenger config))))
                 (index (cadr ((record 'get) (append chain-path '(index)))))
                 (i-remote (cadr ((record 'get) (append chain-path `(*peers* ,name index))))))
            (messenger (,signature-sign `(ledger-prove ,remote-path ,i-remote))))))

     (define ledger-get
       `(lambda*
         (record path index)
         (let ((chain-path (,ledger-path record index)))
           (if (not (and (> (length path) 1) (not (null? path)) (eq? (car path) '*peers*)))
               (let ((index (cadr ((record 'get) (append chain-path '(index))))))
                 (cond ((null? path) '(directory (*state* *peers*) #t))
                       ((eq? (car path) '*state*) ((record 'get) (append `(ledger states ,index) (cdr path))))
                       ((eq? (car path) '*peers*) ((record 'get) (append chain-path '(*peers*))))
                       (else (error 'path-error "Could not get path"))))
               (let ((result (,peer-prove record (cadr path) chain-path (list-tail path 2)))
                     (path-peer (append chain-path `(*peers* ,(cadr path)))))
                 ((record 'deserialize!) '(control scratch) result)
                 (if (not ((record 'equivalent?) path-peer '(control scratch)))
                     (error 'integrity-error "Data does not verify"))
                 ((record 'get) (append '(control scratch) (list-tail path 2))))))))

     (define ledger-set!
       `(lambda (record path value)
          (if (or (null? path) (not (eq? (car path) '*state*)))
              (error 'path-error "first path must be *state*")
              ((record 'set!) (append '(ledger stage) path) value))))

     (define ledger-peer!
       '(lambda (record name messenger)
          ((record 'set!) `(ledger stage *peers* ,name) #f)
          (if (not messenger) ((record 'set!) `(ledger meta peers ,name) #f)
              ((record 'set!) `(ledger meta peers ,name)
               `((messenger ,messenger)
                 ,(assoc 'public-key ((eval messenger) '(ledger-config))))))))

     (define ledger-peers
       '(lambda (record)
          (let ((names (cadr ((record 'get) '(ledger meta peers)))))
            (let loop ((names names) (result '()))
              (if (null? names) (reverse result)
                  (let ((info (cadr ((record 'get) `(ledger meta peers ,(car names))))))
                    (loop (cdr names) (cons (cons (car names) info) result))))))))

     (define ledger-prove
       `(lambda (record path index assertion)
          (if (not (,signature-verify `(ledger-prove ,path ,index) assertion))
              (error 'peer-error "Could not verify assertion")
              (let* ((chain-path (,ledger-path record index))
                     (index (cadr ((record 'get) (append chain-path '(index))))))
                ((record 'copy!) chain-path '(control scratch local))
                ((record 'merge!) `(ledger states ,index) '(control scratch local *state*))
                (if (and (> (length path) 1) (eq? (car path) '*peers*))
                    (let* ((path-remote (list-tail path 2))
                           (result (,peer-prove record (cadr path) chain-path path-remote))
                           (path-root `(control scratch local *peers* ,(cadr path))))
                      ((record 'deserialize!) '(control scratch remote) result)
                      (if (not ((record 'equivalent?) '(control scratch remote) path-root))
                          (error 'peer-error "Ledger integrity error")
                          ((record 'merge!) '(control scratch remote) path-root))))
                ((record 'align!) '(control scratch local))
                ((record 'slice!) '(control scratch local) path)
                (let* ((path-abs (append '(control scratch local) path))
                       (value ((record 'get) path-abs)))
                  (if (eq? (car value) 'directory)
                      (let loop ((names (cadr value)))
                        (if (null? names) #t
                            (begin ((record 'prune!) path-abs `(,(car names)) #t)
                                   (loop (cdr names)))))))
                ((record 'serialize) '(control scratch local))))))

     (define ledger-synchronize
       `(lambda (record assertion)
          (if (not (,signature-verify `(ledger-synchronize) assertion))
              (error 'peer-error "Could not verify assertion")
              (let ((chain-path (,ledger-path record -1)))
                ((record 'copy!) '(ledger chain) '(control scratch))
                ((record 'align!) '(control scratch))
                ((record 'slice!) '(control scratch) '(index))
                ((record 'serialize) '(control scratch))))))

     (let* ((seed (byte-vector->hex-string
                   (sync-hash (expression->byte-vector secret))))
            (result (sync-http 'get (append ,cryptography "/signature/key/" seed)))
            (key-pair (byte-vector->expression result)) 
            (public-key (hex-string->byte-vector (cadr (assoc 'public key-pair))))
            (secret-key (hex-string->byte-vector (cadr (assoc 'private key-pair)))))
       ((record 'set!) '(ledger meta config)
        (list (list 'window ,window)
              (list 'cryptography ,cryptography)
              (list 'public-key public-key)
              (list 'secret-key secret-key))))

     ((record 'set!) '(ledger chain index) 0) 
     ((record 'set!) '(control step 0) step!)
     ((record 'set!) '(control local ledger-config) ledger-config-local)
     ((record 'set!) '(control local ledger-set!) ledger-set!)
     ((record 'set!) '(control local ledger-get) ledger-get)
     ((record 'set!) '(control local ledger-index) ledger-index)
     ((record 'set!) '(control local ledger-peer!) ledger-peer!)
     ((record 'set!) '(control local ledger-peers) ledger-peers)
     ((record 'set!) '(control remote ledger-config) ledger-config-remote)
     ((record 'set!) '(control remote ledger-prove) ledger-prove)
     ((record 'set!) '(control remote ledger-synchronize) ledger-synchronize)

     "Installed ledger"))
