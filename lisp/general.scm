(macro (clear? secret-1 secret-2 window control standard chain tree configuration ledger . classes)

  ;; install control logic
  (sync-call `(,control ,secret-1 ,clear?) #t)

  (define (call function)
    (sync-call `(*call* ,secret-1 ,function) #t))

  (define (query function)
    (sync-call `(*set-query* ,secret-1 ,function) #t))

  ;; install and instantiate standard library
  (call `(lambda (root)
           (let ((init (caddr ,standard)))
             ((root 'set!) '(control class standard) ,standard)
             ((root 'set!) '(control object standard)
              (((eval `(lambda* ,(cddadr init) ,@(cddr init))) ,standard))))))

  ;; install required classes
  (call `(lambda (root) ((root 'set!) '(control class chain) ,chain)))
  (call `(lambda (root) ((root 'set!) '(control class tree) ,tree)))
  (call `(lambda (root) ((root 'set!) '(control class configuration) ,configuration)))
  (call `(lambda (root) ((root 'set!) '(control class ledger) ,ledger)))

  ;; install optional classes
  (let loop ((classes classes))
    (if (null? classes) #t
        (begin (call `(lambda (root ((root 'set!) '(control object ,(caar classes)) ,(cadadr classes)))))
               (loop (cdr classes)))))

  (call `(lambda (root)
           (let* ((std-node ((root 'get) '(control object standard)))
                  (standard ((eval (byte-vector->expression (sync-car std-node))) std-node))
                  (config-class ((root 'get) '(control class configuration)))
                  (standard-class ((root 'get) '(control class standard)))
                  (tree-class ((root 'get) '(control class tree)))
                  (chain-class ((root 'get) '(control class chain)))
                  (ledger-class ((root 'get) '(control class ledger)))
                  (keys (crypto-generate (expression->byte-vector ,secret-2))))
             (if ,clear?
                 (let* ((config-expr `((public ((window ,,window) (public-key ,(car keys))))
                                       (private ((secret-key ,(cdr keys))))))
                        (config ((standard 'make) config-class `(,config-expr)))
                        (ledger ((standard 'make) ledger-class `(,standard ,config ,tree-class ,chain-class))))
                   ((root 'set!) '(control object ledger) (ledger)))
                 (let* ((ledger-old ((standard 'load) ((root 'get) '(control object ledger))))
                        (ledger ((standard 'load) (sync-cons (sync-car (((standard 'make) ledger-class #f))) (ledger-old '(1)))))
                        (recode (lambda (code)
                                  (lambda (obj)
                                    ((standard 'load) (sync-cons (sync-car (((standard 'make) code))) (obj '(1))))))))
                   ((ledger 'update-config!) '(public window) ,window)
                   ((ledger 'update-config!) '(public public-key) (car keys))
                   ((ledger 'update-config!) '(private secret-key) (cdr keys))
                   ((ledger 'update-code!) 'standard (recode standard-class))
                   ((ledger 'update-code!) 'config (recode config-class))
                   ((ledger 'update-code!) 'tree (recode tree-class))
                   ((ledger 'update-code!) 'chain (recode chain-class))
                   ((root 'set!) '(control object ledger) (ledger)))))))

  ;; define secret store
  (call `(lambda (root)
           ((root 'set!) '(interface secret) (sync-hash (expression->byte-vector ,secret-2)))))

  (query
   '(lambda (root query)
      (if (equal? query '(*api*)) ,api-help
          (let ((func (assoc 'function query))
                (args (assoc 'arguments query))
                (auth (assoc 'authentication query))
                (public '(size synchronize resolve information))
                (restricted '(configuration bridges set! get pin! unpin!
                                            general-bridge! general-batch!
                                            step-chain! step-bridge! *secret* *step*)))
            (if (not (or (memq (cadr func) public) (memq (cadr func) restricted)))
                (error 'function-error "Function not recognized by general interface"))
            (if (and (not (memq (cadr func) public))
                     (not (equal? (sync-hash (expression->byte-vector (cadr auth)))
                                  ((root 'get) '(interface secret)))))
                (error 'authentication-error "Could not authenticate restricted interface call"))
            (case (cadr func)
              ((*secret*)
               (let ((secret-new (cadr (assoc 'secret (cadr args)))))
                 ((root 'set!) '(interface secret) (sync-hash (expression->byte-vector secret-new)))))
              ((*step*)
               (let* ((node ((root 'get) '(control object ledger)))
                      (ledger ((eval (byte-vector->expression (sync-car node))) node)))
                 (let loop ((sub-steps ((ledger 'step-generate))))
                   (cond ((null? sub-steps) ((ledger 'size)))
                         ((eq? (caar sub-steps) 'step-chain!)
                          (sync-call `((function ,(caar sub-steps))
                                       (authentication ,(cadr auth))) #f)
                          (loop (cdr sub-steps)))
                         ((eq? (caar sub-steps) 'step-bridge!)
                          (sync-call `((function ,(caar sub-steps))
                                       (arguments ((name ,(cadar sub-steps))))
                                       (authentication ,(cadr auth))) #f)
                          (loop (cdr sub-steps)))
                         (else (error 'step-error "Cannot execute unrecognized substep"))))))
              ((general-bridge!)
               (let* ((node ((root 'get) '(control object ledger)))
                      (ledger ((eval (byte-vector->expression (sync-car node))) node))
                      (name (cadr (assoc 'name (cadr args))))
                      (interface (cadr (assoc 'interface (cadr args))))
                      (result ((ledger 'bridge!) name
                               `((information (lambda ()
                                                (sync-remote ,interface
                                                             '((function information)))))
                                 (synchronize (lambda (index)
                                                (sync-remote ,interface
                                                             `((function synchronize)
                                                               (arguments ((index ,index)))))))
                                 (resolve (lambda (source target)
                                            (sync-remote ,interface
                                                         `((function resolve)
                                                           (arguments ((index ,source) (path ,target)))))))))))
                 ((root 'set!) '(control object ledger) (ledger)) result))
              ((general-batch!)
               (let* ((node ((root 'get) '(control object ledger)))
                      (ledger ((eval (byte-vector->expression (sync-car node))) node))
                      (requests (cadr (assoc 'requests (cadr args)))))
                 (let loop ((requests requests) (output '()))
                   (if (null? requests) (begin ((root 'set!) '(control object ledger) (ledger)) (reverse output))
                       (let ((func (assoc 'function (car requests))))
                         (if (not (or (memq (cadr func) public) (memq (cadr func) restricted)))
                             (error 'function-error "Function not recognized by general interface"))
                         (let* ((args (assoc 'arguments (car requests)))
                                (flat (let loop ((in (if args (reverse (cadr args)) '())) (out '()))
                                        (if (null? in) out
                                            (loop (cdr in) (append `(,(symbol->keyword (caar in)) ,(cadar in)) out)))))
                                (result (apply (ledger (cadr func)) flat)))
                           (loop (cdr requests) (cons result output))))))))
              (else
               (let* ((node ((root 'get) '(control object ledger)))
                      (ledger ((eval (byte-vector->expression (sync-car node))) node))
                      (flat (let loop ((in (if args (reverse (cadr args)) '())) (out '()))
                              (if (null? in) out
                                  (loop (cdr in) (append `(,(symbol->keyword (caar in)) ,(cadar in)) out)))))
                      (result (apply (ledger (cadr func)) flat)))
                 ((root 'set!) '(control object ledger) (ledger)) result)))))))

  "Installed general interface")
