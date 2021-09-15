# How to install Infer from source

## Binary versions

We provide a binary release of Infer. We encourage you to use this
release as compiling infer's dependencies is time-consuming. Follow
the instructions in our [Getting
Started](http://fbinfer.com/docs/getting-started/#get-infer)
page to install Infer.


## Infer dependencies for MacOSX

Here are the prerequisites to be able to compile Infer on MacOSX. This
is required to compile everything from source.

- opam >= 2.0.0 (instructions [here](https://opam.ocaml.org/doc/Install.html#OSX))
- sqlite
- pkg-config
- Java (only needed for the Java analysis)
- CMake (only needed for the C/Objective-C analysis)
- Ninja (optional, if you wish to use sequential linking when building the
  C/Objective-C analysis)
- clang in Xcode command line tools. You can install them with the command
  `xcode-select --install` (only needed for the C/Objective-C analysis)
- Xcode >= 6.1 (only needed for the C/Objective-C analysis)
- autoconf >= 2.63 and automake >= 1.11.1 (if building from git)
- gmp
- mpfr

You can install some of these dependencies using
[Homebrew](http://brew.sh/):

```sh
brew install autoconf automake cmake opam pkg-config sqlite gmp mpfr java
```


## Infer dependencies for Linux

Here are the prerequisites to be able to compile Infer on Linux. This
is required to compile everything from source.

- opam >= 2.0.0
- pkg-config
- Java (only needed for the Java analysis)
- gcc >= 5.X or clang >= 3.4 (only needed for the C/Objective-C analysis)
- autoconf >= 2.63 and automake >= 1.11.1 (if building from git)
- CMake (only needed for the C/Objective-C analysis)
- Ninja (optional, if you wish to use sequential linking when building the
  C/Objective-C analysis)

See also the distro-specific instructions for Ubuntu and Debian below.


## Install Infer from source

Run the following commands to get Infer up and running:

```sh
# Checkout Infer
git clone https://github.com/facebook/infer.git
cd infer
# Compile Infer
./build-infer.sh java
# install Infer system-wide...
sudo make install
# ...or, alternatively, install Infer into your PATH
export PATH=`pwd`/infer/bin:$PATH
```

Replace `./build-infer.sh java` with `./build-infer.sh clang` to build
the C and Objective-C analyzer from source. Beware that this command
may take a really long time because it will compile a custom version
of clang. This custom version is used by Infer to parse C and
Objective-C source code.

See `./build-infer.sh --help` for more options, eg `./build-infer.sh`
on its own will build the analyzers for both Java and C/ObjC.


## Install Infer from source without opam

If for some reason you prefer to install Infer's OCaml dependencies by
some means other than opam, you can still compile Infer by running:

```sh
./autogen.sh
./configure # Disable Java or C/C++/ObjC analyzers with --disable-java-analyzers or --disable-c-analyzers
make
# install Infer system-wide...
sudo make install
# ...or, alternatively, install Infer into your PATH
export PATH=`pwd`/infer/bin:$PATH
```


## How to install the dependencies on Linux

See the Dockerfile in docker/ for inspiration. It includes the
dependencies needed to build Infer on Debian 9 (stretch).


### Setting up opam

Get opam from your distribution, or from the
[opam website](http://opam.ocaml.org/doc/Install.html#Binarydistribution).

The OCaml dependencies needed by Infer are automatically handled by
opam when running `./build-infer.sh`.
