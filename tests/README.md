# Record Tests

## Setup

First, build or otherwise obtain a version of the [journal SDK](https://github.com/sandialabs/sync-journal).
Make a note of where the executable is on your filepath (e.g., `./target/debug/journal-sdk`)
Next, make sure that a [cryptography service](https://github.com/sandialabs/sync-services) is running at the default ports (8887).

## Run

You can now run the test by executing the following commands.

`$ ./test-record.sh <path/to/journal-sdk>`

`$ ./test-ledger.sh <path/to/journalsdk>`

`$ ./test-ontology.sh <path/to/journalsdk>`
