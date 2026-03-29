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
    output=$($sdk -e "$expr" 2>&1)
    status=$?
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
    \`(lambda (msg)
        (let ((result (sync-call msg #t ,(sync-hash (expression->byte-vector journal)))))
          (if (not (and (list? result) (not (null? result)) (eq? (car result) 'error))) result
              (begin (print result)
                     (error 'message-error \"Message returned an error\"))))))"

control=$( cat "$LISP_DIR/control.scm" )
standard=$( cat "$LISP_DIR/standard.scm" )
linear_chain=$( cat "$LISP_DIR/linear-chain.scm" )
log_chain=$( cat "$LISP_DIR/log-chain.scm" )
tree=$( cat "$LISP_DIR/tree.scm" )
config=$( cat "$LISP_DIR/configuration.scm" )
ledger=$( cat "$LISP_DIR/ledger.scm" )
general=$( cat "$LISP_DIR/general.scm" )

run_case "Control Test" "($( cat "$SCRIPT_DIR/test-control.scm" ) $run $messenger '$control)"

run_case "Standards Test" "($( cat "$SCRIPT_DIR/test-standard.scm" ) $run $messenger '$control '$standard)"

run_case "Chain Test" "($( cat "$SCRIPT_DIR/test-chain.scm" ) $run $messenger '$control '$standard '$linear_chain '$log_chain)"

run_case "Tree Test" "($( cat "$SCRIPT_DIR/test-tree.scm" ) $run $messenger '$control '$standard '$tree)"

# run_case "Ledger Test" "($( cat "$SCRIPT_DIR/test-ledger.scm" ) $run $messenger '$control '$standard '$log_chain '$tree '$config '$ledger)"

run_case "General Test" "($( cat "$SCRIPT_DIR/test-general.scm" ) $run $messenger '$general '$control '$standard '$log_chain '$tree '$config '$ledger)"
