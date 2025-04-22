#!/bin/bash

sdk=$1

test=$( cat test-ledger.scm )
record=$( cat ../lisp/record.scm )
control=$( cat ../lisp/control.scm )
ledger=$( cat ../lisp/ledger.scm )

$sdk -e "($test '$record '$control $ledger)"
