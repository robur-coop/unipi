
# Running a the performance test of `unipi` 

## Test dependencies

You need to install the following, e.g. via your package manager (these dependencies adds on top of the `unipi` dependencies):
* `siege` 
* `git`

## Running

Run the following from the root of the repository:
```bash
perftest/locally.sh [--target=TARGET]
```

Where `TARGET` is one of (defaulting to `hvt`):
* `hvt`
* `spt`
* `unix`

It will call `sudo` for setting up and tearing down the network bridge for the unikernel if you test a non-`unix` target. You need to compile the requested target beforehand.



