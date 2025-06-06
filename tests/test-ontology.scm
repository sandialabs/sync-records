(lambda (record-src control-src ledger-src ontology-src)

  (define journal-1 (make-byte-vector 32 1))
  (define journal-2 (make-byte-vector 32 2))
  (define journal-3 (make-byte-vector 32 3))

  (map (lambda (x)
         (if (or (equal? x journal-1)
                 (equal? x journal-2)
                 (equal? x journal-3))
             (sync-delete x)))
       (sync-all))

  (sync-create journal-1)
  (sync-create journal-2)
  (sync-create journal-3)

  (define pass `((,journal-1 . "pass-1")
                 (,journal-2 . "pass-2")
                 (,journal-3 . "pass-3")))
  
  (define output
    (list
     (sync-call `(,record-src
                  ,(cdr (assoc journal-1 pass))
                  ,control-src
                  ,(ledger-src "http://localhost:8887" #t 10)
                  ,ontology-src)
                #t
                journal-1)
     (sync-call `(,record-src
                  ,(cdr (assoc journal-2 pass))
                  ,control-src
                  ,(ledger-src "http://localhost:8887" #t 10)
                  ,ontology-src)
                #t
                journal-2)
     (sync-call `(,record-src
                  ,(cdr (assoc journal-3 pass))
                  ,control-src
                  ,(ledger-src "http://localhost:8887" #t 10)
                  ,ontology-src)
                #t
                journal-3)))

  (define (make-messenger record-id)
    `(lambda (message)
       (sync-call message #t ,record-id)))

  (define input
    `((,journal-1 . *step*)
      (,journal-1 . (ledger-set! (*state* do pin this) 'yes))
      (,journal-1 . (ledger-set! (*state* do pin that) 'yes))
      (,journal-1 . (ledger-set! (*state* do not pin) 'no))
      (,journal-1 . (ontology-insert! (ref (s1)) (ref (p1)) (ref (o1))))
      (,journal-1 . (ontology-insert! (ref (s1)) (ref (p1)) (ref (o5))))
      (,journal-1 . *step*)
      (,journal-1 . (ontology-insert! (ref (s1)) (ref (p1)) (ref (o3))))
      (,journal-1 . *step*)
      (,journal-1 . (ontology-insert! (ref (s1)) (ref (p1)) (ref (o4))))
      (,journal-1 . *step*)
      (,journal-1 . (ontology-select (var) (var) (var)))
      (,journal-1 . (ontology-insert! (ref (s1)) (ref (p1)) (ref (o3))))
      (,journal-1 . (ontology-insert! (ref (s1)) (ref (p1)) (var)))
      (,journal-1 . (ontology-insert! (ref (s1)) (ref (p2)) (exp (+ 2 2))))
      (,journal-1 . (ontology-remove! (ref (s1)) (ref (p1)) (ref (o1))))
      (,journal-1 . *step*)
      (,journal-1 . (ontology-select (ref (s1)) (ref (p1)) (var)))
      (,journal-1 . (ontology-select (ref (s1)) (ref (p2)) (var)))
      (,journal-1 . (ontology-remove! (ref (s1)) (ref (p2)) (exp (+ 2 2))))
      (,journal-1 . *step*)
      (,journal-1 . (ontology-select (ref (s1)) (ref (p2)) (var)))
      (,journal-1 . (ontology-select (ref (s1)) (ref (p2)) (var) #f -2))
      (,journal-1 . (ontology-insert! (ref (s1)) (ref (p2)) (exp (+ 4 4)) (*state* my-ontology)))
      (,journal-1 . (ontology-insert! (ref (s1)) (ref (p2)) (exp (+ 8 8)) (*state* my-ontology)))
      (,journal-1 . *step*)
      (,journal-1 . (ontology-select (var) (var) (var) (*state* my-ontology)))
      (,journal-1 . (ontology-select (var) (var) (exp "none") (*state* my-ontology)))
      (,journal-1 . *step*)
      (,journal-2 . *step*)
      (,journal-2 . *step*)
      (,journal-3 . *step*)
      (,journal-2 . (ledger-peer! journal-1 ,(make-messenger journal-1)))
      (,journal-3 . (ledger-peer! journal-2 ,(make-messenger journal-2)))
      (,journal-1 . *step*)
      (,journal-2 . *step*)
      (,journal-3 . *step*)
      (,journal-1 . *step*)
      (,journal-2 . *step*)
      (,journal-3 . *step*)
      (,journal-2 . (ontology-select (var) (var) (var) (*peers* journal-1 *state* *ontology*)))
      (,journal-3 . (ontology-select (var) (var) (var) (*peers* journal-2 *peers* journal-1 *state* *ontology*)))
      ))

  (let loop ((i 0) (input input) (output output))
    (if (null? input) (reverse output)
        (loop (+ i 1) (cdr input)
              (if (eq? (cdar input) '*step*)
                  (cons (sync-call `(*step* ,(cdr (assoc (caar input) pass)))
                                   #t (caar input)) output)
                  (cons (sync-call `(*local* ,(cdr (assoc (caar input) pass)) ,(cdar input))
                                   #t (caar input)) output))))))
