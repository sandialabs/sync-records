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
                         (append \"Query \`\" (object->string query)
                                 \" returned \`\" (object->string result)
                                 \"\` which failed assertion \`\" (object->string condition)
                                 \"\`\"))
                  (loop (cdr input)))))))))"

messenger="(lambda (journal) 
    \`(lambda (msg) (sync-call msg #t ,(sync-hash (expression->byte-vector journal)))))"

record=$( cat ../lisp/record.scm )
control=$( cat ../lisp/control.scm )
ledger=$( cat ../lisp/ledger.scm )
ontology=$( cat ../lisp/ontology.scm )

echo "--- Record Test ---"
$sdk -e "($( cat ./test-record.scm ) $run $messenger '$record '$control)"

echo "--- Ledger Test ---"
$sdk -e "($( cat ./test-ledger.scm ) $run $messenger '$record '$control '$ledger)"

echo "--- Ontology Test ---"
$sdk -e "($( cat ./test-ontology.scm ) $run $messenger '$record '$control '$ledger '$ontology)"
