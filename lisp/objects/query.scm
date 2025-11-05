;; thoughts
;; - maybe stop worry about remote writing... unclear how that will ever make sense
;; - but we need remote set for data structures, so still necessary
;; - I think remote operations just needs to be explicit...
;;   - at least for a ledger

;; what does trust lie with?
;; - the objects?
;;   - easiest but implies we need a whitelist of objects... ewww
;; - the environment?
;;   - implies we need to lock down remote operations

;; question: what is the minimum we can lock down?
;; - remote operations
;; - access to *sync-state*
;; - access to sync-random
;; - 

;; should we maintain some type of cache? it is deterministic, I think
(obj '(-1 c d -5))

((obj '(1 c d -5) truncate!) '(a b c))

(set! (obj '(a b c d e)) 5)

(set! (obj '(a b peer c d)) 'latest)

(let ((remote (chain '(-1 c d e))))
  ((remote '(a b c d))
  ((remote 'fetch!) messenger '(a b c d))
