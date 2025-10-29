(lambda (control-src standard-src chain-src path start end step)
  (let* ((journal (sync-hash (expression->byte-vector 'journal)))
         (password "password")
         (instantiate (lambda* (name path)
                               `(*call* ,password
                                        (lambda (root)
                                          (let* ((std-node (cadr ((root 'get) '(control library standard))))
                                                 (std ((eval (byte-vector->expression (sync-car std-node))) std-node))
                                                 (cls (cadr ((root 'get) ',path)))
                                                 (obj ((std 'make) cls)))
                                            ((root 'set!) '(control test ,name) `(content ,(obj))))))))
         (query (lambda (name expression)
                  `(*call* ,password
                           (lambda (root)
                             (let* ((node (cadr ((root 'get) '(control test ,name))))
                                    (,name ((eval (byte-vector->expression (sync-car node))) node))
                                    (start (time-unix))
                                    (result ,expression)
                                    (end (time-unix)))
                               ((root 'set!) '(control test ,name) `(content ,(,name)))
                               (- end start)))))))
    (sync-create journal)
    (sync-call `(,control-src ,password) #t journal)
    (sync-call `(*call* ,password ,standard-src) #t journal)
    (sync-call `(*call* ,password ,chain-src) #t journal)
    (sync-call (instantiate 'chain path) #t journal)
    (sync-call (query 'chain
                      `(let loop ((index 0))
                         (if (>= index ,start) #t
                             (begin ((chain 'push!) (expression->byte-vector index))
                                    (loop (+ index 1)))))) #t journal)
    `((push
       ,(let loop ((index start) (result '()))
          (if (>= index end) result
              (let ((r (sync-call (query 'chain `((chain 'push!) (expression->byte-vector (+ ,index ,start)))) #t journal)))
                (if (> step 0) (sync-call (query 'chain
                                                 `(let loop ((i 1))
                                                    (if (>= i ,step) #t
                                                        (begin ((chain 'push!) (expression->byte-vector (+ ,index i)))
                                                               (loop (+ i 1)))))) #t journal))
                (loop (+ index step) (cons r result))))))
      (get
       ,(let loop ((index 0) (result '()))
          (if (>= index (- end start)) (reverse result)
              (let ((r (sync-call (query 'chain `((chain 'get) (- -1 ,index))) #t journal)))
                (loop (+ index step) (cons r result))))))
      (digest
       ,(let loop ((index 0) (result '()))
          (if (= index (- end start)) (reverse result)
              (let ((r (sync-call (query 'chain `((chain 'digest) (- -1 ,index))) #t journal)))
                (loop (+ index step) (cons r result)))))))))
