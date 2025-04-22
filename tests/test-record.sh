#!/bin/bash

sdk=$1

script="(lambda (boot query)
           (define record (make-byte-vector 32 1))
           (map (lambda (x) (if (equal? x record) (sync-delete record))) (sync-all))
           (sync-create record)
           (let loop ((query query) (result (list (sync-call boot #t record))))
             (if (null? query) (reverse result)
                 (let ((r (sync-call (car query) #t record)))
                   (loop (cdr query) (cons r result))))))"

shell="(lambda (record secret)
  ((record 'set!) '(control remote test) 
   '(lambda (record query) (eval query))))"

boot="'($( cat ../lisp/record.scm ) \"password\" $( cat ../lisp/control.scm ) $shell)"

query="'$( cat test-record.scm )"

$sdk -e "($script $boot $query)"
