#!/bin/bash

sdk=$1
secret="password"
cryptography="http://localhost:8887" 

record=$( cat ../lisp/record.scm )
control=$( cat ../lisp/control.scm )
ledger=$( cat ../lisp/ledger.scm )
ontology=$( cat ../lisp/ontology.scm )

boot="'($record \"$secret\" $control ($ledger \"$cryptography\" #t #f) $ontology)"

tests="($( cat test-ontology.scm ) \"$secret\")"

script="(lambda (boot tests)
           (define record (make-byte-vector 32 1))
           (map (lambda (x) (if (equal? x record) (sync-delete record))) (sync-all))
           (sync-create record)
           (let loop ((tests tests) (result (list (sync-call boot #t record))))
             (if (null? tests) (reverse result)
                 (let ((r (sync-call (car tests) #t record)))
                   (loop (cdr tests) (cons r result))))))"

$sdk -e "($script $boot $tests)"
