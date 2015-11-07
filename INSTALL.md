# How to compile and install Infer

## Pre-compiled versions

We provide a source release of Infer packaged with pre-build binaries
for clang and facebook-clang-plugins for Linux and MacOS. We encourage
you to use this release as compiling clang is time-consuming. Install
the dependencies as explained in the next section, then follow the
instructions in [Infer's getting-started
page](http://fbinfer.com/docs/getting-started.html) to compile and
install Infer.

Alternatively, we also provide a docker image in the docker/
directory. Simply go to that directory and run `./run.sh` to get
started with a working installation of Infer.


## Infer dependencies for MacOSX

Here are the prerequisites to be able to compile Infer on MacOSX. This
is required to be able to [use the
release](http://fbinfer.com/docs/getting-started.html) (faster), or to
compile everything from source (see the end of this document).

- opam (instructions [here](https://opam.ocaml.org/doc/Install.html#OSX))
- Python 2.7
- Java 1.7 (only needed for the Java analysis)
- clang in Xcode command line tools. You can install them with the command
  `xcode-select --install` (only needed for the C/Objective-C analysis)
- Xcode <= 7.0, >= 6.1 (only needed for the C/Objective-C analysis)

You can install some of these dependencies using
[Homebrew](http://brew.sh/):

```sh
brew install opam
```

Once you have all the dependencies above installed, configure opam as
follows:

```sh
opam init -y --comp=4.01.0
eval $(opam config env)
opam update
opam install -y \
  atdgen.1.6.0 \
  extlib.1.5.4 \
  javalib.2.3.1 \
  sawja.1.5.1
```


## Infer dependencies for Linux

Here are the prerequisites to be able to compile Infer on Linux. This
is required to be able to [use the
release](http://fbinfer.com/docs/getting-started.html) (faster), or to
compile everything from source (see the end of this document).

- gcc >= 4.7.2
- opam
- Python 2.7
- Java 1.7 (only needed for the Java analysis)

### How to install the dependencies on Ubuntu 12.04.4 LTS

```sh
sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install -y \
  build-essential \
  g++-4.8 \
  gcc-4.8 \
  git \
  libgmp-dev \
  libmpc-dev \
  libmpfr-dev \
  m4 \
  openjdk-7-jdk \
  python-software-properties \
  unzip \
  zlib1g-dev
sudo update-alternatives \
  --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 60 \
  --slave /usr/bin/g++ g++ /usr/bin/g++-4.8
```

### How to install the dependencies on Debian 7 / Ubuntu 14.04

```sh
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install -y \
  build-essential \
  git \
  libgmp-dev \
  libmpc-dev \
  libmpfr-dev \
  m4 \
  openjdk-7-jdk \
  python-software-properties \
  unzip \
  zlib1g-dev
```

### Setting up Opam

Unfortunately, the version of opam that ships with some Linux
distributions is broken, so you'll have to get it from the web:

```sh
wget -O opam https://github.com/ocaml/opam/releases/download/1.2.2/opam-1.2.2-x86_64-Linux
chmod +x opam
```

Alternatively, follow the instructions [from the opam
webpage](https://opam.ocaml.org/doc/Install.html).

Once opam is installed, run the following commands:

```sh
./opam init -y --comp=4.01.0
eval $(./opam config env)
./opam update
./opam install -y \
  atdgen.1.6.0 \
  extlib.1.5.4 \
  javalib.2.3.1 \
  sawja.1.5.1
```


## Compile Infer from source for the Java analysis

If you use Infer to analyze Java programs only, you can simple use
these steps to get Infer up and running:

```sh
# Checkout Infer
git clone https://github.com/facebook/infer.git
cd infer
# Compile Infer
make -C infer java
# Install Infer into your PATH
export PATH=`pwd`/infer/bin:$PATH
```


## Compile Infer from source with clang enabled

You do not need to follow the instructions below if you are using a
[release of Infer](http://fbinfer.com/docs/getting-started.html) or if
you only need to run Infer on Java programs.

Infer uses a special version of clang along with a clang
plugin. Follow these steps to compile them from source and install
Infer.

```sh
# Checkout Infer
git clone https://github.com/facebook/infer.git
cd infer
git submodule update --init --recursive
# Compile clang
facebook-clang-plugins/clang/setup.sh # go have a coffee :)
# Compile the clang plugin
./compile-fcp.sh
# Compile Infer
make -C infer
# Install Infer into your PATH
export PATH=`pwd`/infer/bin:$PATH
```
