(lambda (run-test make-messenger general-src control-src standard-src chain-src tree-src config-src ledger-src)
  (let* ((pass (lambda (x) (append "pass-" (symbol->string x))))
         (install (lambda (x) `(,x (,general-src #t ,(pass x) ,(pass x) 1024 ,control-src ',standard-src ',chain-src ',tree-src ',config-src ',ledger-src) "Installed general interface")))
         (update (lambda (x) `(,x (*eval* ,(pass x) (,general-src #f ,(pass x) ,(pass x) 512 ,control-src ',standard-src ',chain-src ',tree-src ',config-src ',ledger-src)) "Installed general interface")))
         ;; (update (lambda (x) `(,x (*eval* ,(pass x) (+ 2 2)) "Installed general interface")))
         (form (lambda (x) `(,(car x) (*call* ,(pass (car x)) (lambda (root) ,(cadr x))) ,@(cddr x)))))
    (run-test
     (append
      (map install '(journal))
      (map form 
           `((journal (let* ((ledger-node ((root 'get) '(control object ledger)))
                             (ledger ((eval (byte-vector->expression (sync-car ledger-node))) ledger-node)))
                        ((ledger 'configuration) '(public window))) 1024)))
      (map update '(journal))
      (map form 
           `((journal (let* ((ledger-node ((root 'get) '(control object ledger)))
                             (ledger ((eval (byte-vector->expression (sync-car ledger-node))) ledger-node)))
                        ((ledger 'configuration) '(public window))) 512)))
      ))))
