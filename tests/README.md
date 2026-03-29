# Record Tests

## Setup

First, build or otherwise obtain a version of the [journal SDK](https://github.com/sandialabs/sync-journal).
There are two options for this:

### Binary

 Build the binary yourself using the instructions on [journal SDK](https://github.com/sandialabs/sync-journal). If successful, make note of the path to binary (e.g., `./target/debug/journal-sdk`)

### Docker

Use the prebuilt Docker container

`$ docker pull ghcr.io/sandialabs/sync-journal/journal-sdk`

## Run Tests

You can now run the test by providing `test.sh` by passing in access to the SDK.

### Binary

`$ ./test.sh <path/to/journal-sdk>`

### Docker

`$ ./test.sh "docker run ghcr.io/sandialabs/sync-journal/journal-sdk"`

## Develop

The current test suite is mostly built around direct source-driven harnesses.
The top-level `test.sh` file loads the active module source files from `lisp/`, passes them into each `test-*.scm` lambda, and evaluates the resulting expression with the Journal SDK.

Each active `test-*.scm` file is a Scheme function that accepts the source blobs it needs, instantiates local objects or journals, and returns either:

- a success string of the form `"Success (N checks)"`
- or an `(error ...)` form if an assertion fails

Most tests now use a direct style:

- `test-standard.scm`, `test-tree.scm`, and `test-chain.scm`
  - instantiate the needed classes locally with `standard.scm`
  - explicitly `(sync-eval node #f)` constructed nodes when a live object is needed
  - run assertions directly in one evaluation

- `test-ledger.scm`
  - simulates multiple local ledgers inside one evaluation
  - coordinates them directly without real journal transport

- `test-interface.scm`
  - creates multiple real journals with `sync-create`
  - installs `interface.scm` into each journal
  - monkey-patches transport details inside the test so cross-journal flows remain deterministic

When adding a new test, prefer the direct lambda style used by the active suite:

1. accept only the source blobs the test actually needs
2. instantiate objects or journals locally inside the test
3. use `standard 'make` for uninitialized shells and `standard 'init` when constructor args are required; use `(sync-eval node #f)` only when a live object is needed
4. use a small local `assert` helper/macro
5. return a compact success string when all checks pass
