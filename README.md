<img src="website/static/img/logo.png" alt="logo" width="15%" />

# Infer ![build](https://github.com/facebook/infer/actions/workflows/install.yml/badge.svg) ![website](https://github.com/facebook/infer/actions/workflows/deploy.yml/badge.svg)

[Infer](http://fbinfer.com/) is a static analysis tool for Java,
C++, Objective-C, and C. Infer is written in [OCaml](https://ocaml.org/).

## Installation

Read our [Getting
Started](http://fbinfer.com/docs/getting-started) page for
details on how to install packaged versions of Infer. To build Infer
from source, see [INSTALL.md](./INSTALL.md).

### Local development quickstart

```sh
./build-infer.sh
make devsetup
make -j              # default BUILD_MODE=dev (treat warnings as errors)
make -j test_build   # quick sanity check
make -j4 test        # run tests (adjust 4 to your CPU cores)
```

Faster OCaml inner loop:

```sh
make -j -C infer/src check   # type-check only
make -j -C infer/src byte    # fast bytecode build for small examples
```

Temporarily ignore warnings while iterating (clean up before committing):

```sh
make BUILD_MODE=dev-noerror -j
```

Analyze a project you build with Make or Maven:

```sh
infer run -- make <target>
infer run -- mvn <target>
```

See [INSTALL.md](./INSTALL.md) for platform-specific details and the website docs for wrapping real builds.

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md).

## License

Infer is MIT-licensed.

Note: Enabling Java support may require you to download and install 
components licensed under the GPL.
