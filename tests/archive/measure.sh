#!/bin/bash

if [ -z $1 ]; then
    echo "Please provide a path to a Journal SDK executable"
    exit 1
fi

sdk=$1

control=$( cat ../lisp/objects/control.scm )
standard=$( cat ../lisp/objects/standard.scm )
linear_chain=$( cat ../lisp/objects/linear-chain.scm )
skip_chain=$( cat ../lisp/objects/skip-chain.scm )
history_chain=$( cat ../lisp/objects/history-chain.scm )
log_chain=$( cat ../lisp/objects/log-chain.scm )

start=${2:-1024}
end=${3:-2048}
step=${4:-1}

echo "--- Hash Chain Measurement ---"

$sdk -e "($( cat ./measure-chain.scm ) '$control '$standard '$linear_chain '(control library linear-chain) $start $end $step)"

echo "--- Skip List Measurement ---"

$sdk -e "($( cat ./measure-chain.scm ) '$control '$standard '$skip_chain '(control library skip-chain) $start $end $step)"

echo "--- History Log Measurement ---"

$sdk -e "($( cat ./measure-chain.scm ) '$control '$standard '$history_chain '(control library history-chain) $start $end $step)"

echo "--- Log Chain Measurement ---"

$sdk -e "($( cat ./measure-chain.scm ) '$control '$standard '$log_chain '(control library log-chain) $start $end $step)"
