# Synchronic Web Records

This repository contains reusable record artifacts—primarily Lisp/Scheme modules and functions—for use with the [Synchronic Web Journal SDK](https://github.com/sandialabs/sync-journal). These modules provide core logic for ledger, ontology, control, and record management in the Synchronic Web infrastructure.

## Contents

- `lisp/`
  - `record.scm`, `control.scm`, `ledger.scm`, `ontology.scm`, `evaluator.scm`: Core Scheme modules for record operations.
  - `archive/`: (PENDING DEPRECATION) Historical or auxiliary Scheme modules (e.g., `blockchain.scm`, `state.scm`, `utils.scm`)
- `tests/`
  - `test-ledger.scm`, `test-ontology.scm`, `test-record.scm`: Automated test scripts for validating record and ledger logic.
  - `test.sh`: Shell script to run the test suite.
  - `README.md`: Documentation for running and developing tests.

## Usage

These Scheme modules are intended to be loaded into a running Synchronic Web Journal instance, either at startup or dynamically via the API. They provide the logic for advanced record-keeping, provenance, and ledger operations.

### Example: Using with the Journal SDK

1. **Build and run the Journal SDK**  
   See the [sync-journal README](https://github.com/sandialabs/sync-journal) for build instructions.

2. **Load record modules**  
   You can load the provided Scheme files into the journal using the web interface or by passing them as arguments to the SDK, e.g.:
   ```
   ./journal-sdk -b "($( cat path/to/record.scm ))"
   ```

3. **Invoke record/ledger operations**  
   Use the API or web interface to call functions defined in these modules, such as:
   ```
   (ledger-set! (*state* documents arxiv the-synchronic-web) "0x116...")
   (ledger-get (*state* documents arxiv the-synchronic-web))
   ```

## Testing

See `tests/README.md` for more details on running and developing tests.

## Contributing

Contributions of new record modules, bug fixes, and test cases are welcome! Please open issues or pull requests on GitHub.
