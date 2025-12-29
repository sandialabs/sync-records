#!/bin/bash

if [ -z $1 ]; then
    echo "Please provide a path to a Journal SDK executable"
    exit 1
fi

sdk=$1

run="(lambda (script)
  (let ((nodes (hash-table)))
    (let loop ((input script))
      (if (null? input) (append \"Success (\" (object->string (length script)) \" steps)\")
          (let ((journal (caar input))
                (query (cadar input))
                (trunc (lambda (x y) (if (< (length x) y) x (append (substring x 0 y) \" ...\"))))
                (condition (cond ((null? (cddar input)) '(lambda (x) #t))
                               ((and (pair? (caddar input)) (eq? (car (caddar input)) 'lambda))
                                (caddar input))
                               (else \`(lambda (result) (equal? result ,(caddar input)))))))
            (if (not (nodes journal))
                (begin (set! (nodes journal) (sync-hash (expression->byte-vector journal)))
                       (sync-create (nodes journal))))
            (let ((result (sync-call query #t (nodes journal))))
              (if (not ((eval condition) result))
                  (error 'assertion-failure
                         (append \"Query [\" (trunc (object->string query) 256)
                                 \"] returned [\" (trunc (object->string result) 256)
                                 \"] which failed assertion [\" (object->string condition)
                                 \"]\"))
                  (loop (cdr input)))))))))"

messenger="(lambda (journal) 
    \`(lambda (msg) (sync-call msg #t ,(sync-hash (expression->byte-vector journal)))))"

control=$( cat ../lisp/objects/control.scm )
standard=$( cat ../lisp/objects/standard.scm )
linear_chain=$( cat ../lisp/objects/linear-chain.scm )
log_chain=$( cat ../lisp/objects/log-chain.scm )
record=$( cat ../lisp/objects/record.scm )
ledger=$( cat ../lisp/objects/ledger.scm )
# ontology=$( cat ../lisp/ontology.scm )

echo "--- Control Test ---"
$sdk -e "($( cat ./test-control.scm ) $run $messenger '$control)"

echo "--- Standards Test  ---"
$sdk -e "($( cat ./test-standard.scm ) $run $messenger '$control '$standard)"

echo "--- Chain Test ---"
$sdk -e "($( cat ./test-chain.scm ) $run $messenger '$control '$standard '$linear_chain '$log_chain)"

echo "--- Record Test ---"
$sdk -e "($( cat ./test-record.scm ) $run $messenger '$control '$standard '$record)"

# echo "--- Ledger Test ---"
# $sdk -e "($( cat ./test-ledger.scm ) $run $messenger '$control '$standard '$log_chain '$record '$ledger)"

# echo "--- Ontology Test ---"
# $sdk -e "($( cat ./test-ontology.scm ) $run $messenger '$record '$control '$ledger '$ontology)"
