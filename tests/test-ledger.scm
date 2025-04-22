(lambda (record-src control-src ledger-src)

  (define ledger-1 (make-byte-vector 32 1))
  (define ledger-2 (make-byte-vector 32 2))
  (define ledger-3 (make-byte-vector 32 3))
  (define ledger-4 (make-byte-vector 32 4))
  (define ledger-5 (make-byte-vector 32 5))

  (map (lambda (x)
         (if (or (equal? x ledger-1)
                 (equal? x ledger-2)
                 (equal? x ledger-3)
                 (equal? x ledger-4)
                 (equal? x ledger-5))
             (sync-delete x)))
       (sync-all))

  (sync-create ledger-1)
  (sync-create ledger-2)
  (sync-create ledger-3)
  (sync-create ledger-4)
  (sync-create ledger-5)

  (define pass `((,ledger-1 . "pass-1")
                 (,ledger-2 . "pass-2")
                 (,ledger-3 . "pass-3")
                 (,ledger-4 . "pass-4")
                 (,ledger-5 . "pass-5")))
  
  (define output
    (list
     (sync-call `(,record-src
                  ,(cdr (assoc ledger-1 pass))
                  ,control-src
                  ,(ledger-src "http://localhost:8887" #t 1024))
                #t
                ledger-1)
     (sync-call `(,record-src
                  ,(cdr (assoc ledger-2 pass))
                  ,control-src
                  ,(ledger-src "http://localhost:8887" #t 1024))
                #t
                ledger-2)
     (sync-call `(,record-src
                  ,(cdr (assoc ledger-3 pass))
                  ,control-src
                  ,(ledger-src "http://localhost:8887" #t 1024))
                #t
                ledger-3)
     (sync-call `(,record-src
                  ,(cdr (assoc ledger-4 pass))
                  ,control-src
                  ,(ledger-src "http://localhost:8887" #t 1024))
                #t
                ledger-4)
     (sync-call `(,record-src
                  ,(cdr (assoc ledger-5 pass))
                  ,control-src
                  ,(ledger-src "http://localhost:8887" #t 1024))
                #t
                ledger-5)))

  (define (make-messenger record-id)
    `(lambda (message)
       (sync-call message #t ,record-id)))

  (define input
    `((,ledger-1 . *step*)
      (,ledger-1 . *step*)
      (,ledger-1 . *step*)
      (,ledger-1 . (ledger-peer! ledger-2 ,(make-messenger ledger-2)))
      (,ledger-1 . (ledger-peer! ledger-3 blah))
      (,ledger-1 . (ledger-peer! ledger-3 #f))
      (,ledger-2 . *step*)
      (,ledger-2 . (ledger-set! (*state* a b c) 42))
      (,ledger-2 . (ledger-set! (*state* a b c*) 43))
      (,ledger-2 . *step*)
      (,ledger-2 . (ledger-get (*state* a b c*)))
      (,ledger-1 . *step*)
      (,ledger-2 . *step*)
      (,ledger-1 . *step*)
      (,ledger-2 . *step*)
      (,ledger-1 . *step*)
      (,ledger-2 . *step*)
      (,ledger-2 . (ledger-set! (*state* a b c) 44))
      (,ledger-2 . *step*)
      (,ledger-1 . *step*)
      (,ledger-1 . *step*)
      (,ledger-2 . *step*)
      (,ledger-1 . (ledger-index))
      (,ledger-1 . (ledger-get (*peers* ledger-2 *state* a b)))
      (,ledger-1 . (ledger-get (*peers* ledger-2 *state* a b c) 6))
      (,ledger-1 . (ledger-get (*peers* ledger-2 *state* a b c)))
      (,ledger-2 . (ledger-peer! ledger-3 ,(make-messenger ledger-3)))
      (,ledger-3 . (ledger-peer! ledger-4 ,(make-messenger ledger-4)))
      (,ledger-3 . (ledger-peer! ledger-5 ,(make-messenger ledger-5)))
      (,ledger-3 . (ledger-set! (*state* d e f) 64))
      (,ledger-4 . (ledger-set! (*state* g h i) "hello"))
      (,ledger-5 . (ledger-set! (*state* g h i) "world"))
      (,ledger-1 . *step*)
      (,ledger-2 . *step*)
      (,ledger-3 . *step*)
      (,ledger-4 . *step*)
      (,ledger-5 . *step*)
      (,ledger-1 . *step*)
      (,ledger-2 . *step*)
      (,ledger-3 . *step*)
      (,ledger-4 . *step*)
      (,ledger-5 . *step*)
      (,ledger-2 . (ledger-get (*peers* ledger-3 *state* d e f)))
      (,ledger-1 . *step*)
      (,ledger-2 . *step*)
      (,ledger-3 . *step*)
      (,ledger-4 . *step*)
      (,ledger-5 . *step*)
      (,ledger-1 . *step*)
      (,ledger-2 . *step*)
      (,ledger-3 . *step*)
      (,ledger-4 . *step*)
      (,ledger-5 . *step*)
      (,ledger-1 . (ledger-get (*peers* ledger-2 *peers* ledger-3 *state* d e f)))
      (,ledger-1 . (ledger-get (*peers* ledger-2 *peers* ledger-3 *peers* ledger-4 *state* g h i)))
      (,ledger-1 . (ledger-get (*peers* ledger-2 *peers* ledger-3 *peers* ledger-5 *state* g h i)))
      ))

  (let loop ((i 0) (input input) (output output))
    (if (null? input) (reverse output)
        (loop (+ i 1) (cdr input)
              (if (eq? (cdar input) '*step*)
                  (cons (sync-call `(*step* ,(cdr (assoc (caar input) pass)))
                                   #t (caar input)) output)
                  (cons (sync-call `(*local* ,(cdr (assoc (caar input) pass)) ,(cdar input))
                                   #t (caar input)) output))))))
