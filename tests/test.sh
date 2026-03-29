#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd -- "$SCRIPT_DIR/.." && pwd)"
LISP_DIR="$ROOT_DIR/lisp"

if [ -z "${1:-}" ]; then
    echo "Please provide a path to a Journal SDK executable"
    exit 1
fi

sdk=$1

run_case() {
    name="$1"
    expr="$2"

    echo "--- $name ---"
    set +e
    output=$($sdk -e "$expr" 2>&1)
    status=$?
    set -e
    echo "$output"

    if [ "$status" -ne 0 ]; then
        exit "$status"
    fi

    # Janky but effective: treat interpreter-level "(error ...)" output as test failure.
    first_line=$(printf '%s\n' "$output" | sed -n '/./{p;q;}')
    if [[ "$first_line" == "(error "* ]]; then
        echo "FAIL: $name returned an error form."
        exit 1
    fi
}

control=$( cat "$LISP_DIR/control.scm" )
standard=$( cat "$LISP_DIR/standard.scm" )
linear_chain=$( cat "$LISP_DIR/linear-chain.scm" )
log_chain=$( cat "$LISP_DIR/log-chain.scm" )
tree=$( cat "$LISP_DIR/tree.scm" )
ledger=$( cat "$LISP_DIR/ledger.scm" )
interface=$( cat "$LISP_DIR/interface.scm" )

run_case "Control Test" "($( cat "$SCRIPT_DIR/test-control.scm" ) '$control)"

run_case "Standard Test" "($( cat "$SCRIPT_DIR/test-standard.scm" ) '$standard)"

run_case "Tree Test" "($( cat "$SCRIPT_DIR/test-tree.scm" ) '$standard '$tree)"

run_case "Linear Chain Test" "($( cat "$SCRIPT_DIR/test-chain.scm" ) '$standard '$linear_chain)"

run_case "Log Chain Test" "($( cat "$SCRIPT_DIR/test-chain.scm" ) '$standard '$log_chain)"

run_case "Ledger Test" "($( cat "$SCRIPT_DIR/test-ledger.scm" ) '$standard '$log_chain '$tree '$ledger)"

run_case "Interface Test" "($( cat "$SCRIPT_DIR/test-interface.scm" ) '$control '$standard '$log_chain '$tree '$ledger '$interface)"
