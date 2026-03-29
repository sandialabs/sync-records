# Synchronic Web Records

This repository contains reusable record artifacts, primarily Lisp/Scheme modules and tests, for use with the [Synchronic Web Journal SDK](https://github.com/sandialabs/sync-journal). The active modules here provide the standard object system, storage structures, ledger logic, and authenticated interface layer used by the current sync-record stack.

## Contents

- `lisp/`
  - `control.scm`: Installs the root object and admin-controlled call/query/step hooks.
  - `standard.scm`: The shared object model used by the other modules. Public object boundaries now work primarily on `sync-node` values, with explicit `(sync-eval node #f)` where a live object is needed internally. `make` builds an uninitialized shell and `init` applies `*init*` when constructor arguments are needed.
  - `tree.scm`, `linear-chain.scm`, `log-chain.scm`, `ledger.scm`, `interface.scm`: Active record modules and data structures. `ledger.scm` now stores its configuration directly rather than delegating to a separate configuration class.
  - `archive/`: Historical or auxiliary Scheme modules retained for reference.
- `tests/`
  - Direct test harnesses such as `test-standard.scm`, `test-tree.scm`, `test-chain.scm`, `test-ledger.scm`, and `test-interface.scm`.
  - `test.sh`: Shell script to run the test suite.
  - `README.md`: Documentation for running and developing tests.

## Usage

These Scheme modules are intended to be loaded into a running Synchronic Web Journal instance, either at startup or dynamically via the API. In the current layout, `control.scm` provides the outer journal control layer and `interface.scm` installs the authenticated record interface backed by `ledger.scm` and the supporting classes.

### Example: Using with the Journal SDK

1. **Build and run the Journal SDK**  
   See the [sync-journal README](https://github.com/sandialabs/sync-journal) for build instructions.

2. **Load record modules**  
   You can load the provided Scheme files into the journal using the web interface or by passing them as arguments to the SDK. In practice, `interface.scm` is the entry point that installs and wires together the other active modules.
   For example:
   ```
   ./journal-sdk -e "($( cat lisp/interface.scm ) #t \"admin-pass\" \"interface-pass\" 4 \
     '$( cat lisp/control.scm ) '$( cat lisp/standard.scm ) '$( cat lisp/log-chain.scm ) \
     '$( cat lisp/tree.scm ) '$( cat lisp/ledger.scm ))"
   ```

3. **Invoke record/ledger operations**  
   Use the installed query interface to call functions such as:
   ```
   ((function set!) (arguments ((path ((*state* my data path))) (value 42))) (authentication "interface-pass"))
   ((function get) (arguments ((path ((*state* my data path))))) (authentication "interface-pass"))
   ```

## Notes

- The current codebase prefers `sync-node` values at module boundaries. In particular, `standard 'make` returns an uninitialized shell node, while `standard 'init` returns an initialized node.
- When a caller needs the loaded object form, use `(sync-eval node #f)` explicitly.

## Testing

See `tests/README.md` for more details on running and developing tests.

## Contributing

Contributions of new record modules, bug fixes, and test cases are welcome! Please open issues or pull requests on GitHub.
