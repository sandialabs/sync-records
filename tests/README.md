# Record Tests

## Setup

First, build or otherwise obtain a version of the [journal SDK](https://github.com/sandialabs/sync-journal).
Make a note of where the executable is on your filepath (e.g., `./target/debug/journal-sdk`)
Next, make sure that a [cryptography service](https://github.com/sandialabs/sync-services) is running at the default ports (8887).

## Run

You can now run the test by executing the following commands.

`$ ./test.sh <path/to/journal-sdk>`

## Develop

This test suite supports testing of multiple journals within a discrete and deterministic framework.
The top-level `test.sh` file leverages synchronic web primitives to convert actions and communications between multiple simulated journals into a single lisp-style expression that the SDK can evaluate.
The `*-test.scm` within this directory contain logic for generating and executing scripted actions among multiple journals.
To maximize flexibility, the contents of each `*-test.scm` is actually a function that accepts the following inputs:

- `run-test` a function `(lambda (script) ...)` that takes a test script and runs it
- `make-messenger` a function `(lambda (journal) ...)` that takes a journal name (symbol) and returns a function `(lambda (message) ...)` for sending messages to the journal
- `record-src` the source code for the `record.scm` interface
- `record-src` the source code for the `control.scm` function required by `record.scm`
- `...` other test-specific arguments (e.g., more source code) required to run the test

In general, `*-record.scm` functions can run arbitrary code to compute and format the desired test script.
Eventually, the script that is passed into `run-test` must be a list where each item is itself a list with the following elements in order:

1. `journal`: a symbol refering to the simulated journal
2. `query`: the query to be evaluated by the simulated journal
3. `condition`: (optional) a function or expression to check the output

If the condition is a function, it must have the form `(lambda (result) ...)` to accept the returned result of the query and return an output indicating whether the script action succeeds or fails.
If the condition is an expression, it will be compared against the result to determine success or failure using the `equals?` function.
If the condition is omitted, then the script action is assumed to succeed.
A test passes if all of the individual script actions succeed.
