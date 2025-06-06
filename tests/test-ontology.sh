#!/bin/bash

sdk=$1

test=$( cat test-ontology.scm )
record=$( cat ../lisp/record.scm )
control=$( cat ../lisp/control.scm )
ledger=$( cat ../lisp/ledger.scm )
ontology=$( cat ../lisp/ontology.scm )

$sdk -e "($test '$record '$control $ledger '$ontology)"
