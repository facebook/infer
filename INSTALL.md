# Install Infer

We provide pre-built Infer binaries for Linux and MacOS.  If you just
wish to use Infer, and are not interested in making contributions to
it, then these binaries are all you need.  Otherwise, if you wish to
compile Infer, here are also instructions to do so, depending on your
operating system.

Installing Infer from binaries is described [in the Infer's getting
started page](http://fbinfer.com/docs/getting-started.html).

The rest of this document describes how to install Infer from source
on different platforms.


## Download the Infer repository

```bash
git clone https://github.com/facebook/infer.git
```

To analyse C and Objective-C, Infer requires clang and the
[facebook-clang-plugin](https://github.com/facebook/facebook-clang-plugins). If
you wish to analyse only Java/Android code, then you could skip these
dependencies. Details below.


## MacOS X

### Requirements

- Python 2.7
- [opam](https://opam.ocaml.org/doc/Install.html#OSX)

#### Requirements for the Java analysis

- Java <= 1.7
- Android dev environment setup for analysis of Android apps.

#### Requirements for the C/Objective-C analysis

- XCode <= 6.3, >= 6.1
- clang (in XCode command line tools. You can install them with the command `xcode-select --install`)

### Installation instructions

Install the OCaml dependencies:

```bash
opam init --comp=4.01.0  # (answer 'y' to the question)
eval `opam config env`
opam install extlib.1.5.4 atdgen.1.6.0 javalib.2.3.1 sawja.1.5.1
```

If you do not require support for the C/Objective-C analysis in Infer,
and only wish to analyse Java files, continue with these
instructions. By the way, Java 1.8 is not supported.

```bash
cd infer
make -C infer java
export PATH=`pwd`/infer/bin:$PATH
```

To compile support for both Java and C/Objective-C, do this instead.

```bash
cd infer
./update-fcp.sh && ../facebook-clang-plugin/clang/setup.sh && ./compile-fcp.sh # go have a coffee :)
make -C infer
export PATH=`pwd`/infer/bin:$PATH
```


## Linux

### Requirements

- Python 2.7

### Installation instructions

These instructions were tested on Linux 64 bits on the following
distributions: Debian 7, Ubuntu 14.04 and Ubuntu 12.04.4 LTS.

Install the OCaml dependencies:

```bash
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install git openjdk-7-jdk m4 zlib1g-dev python-software-properties build-essential libgmp-dev libmpfr-dev libmpc-dev unzip
wget https://github.com/ocaml/opam/releases/download/1.2.2/opam-1.2.2-x86_64-Linux -O opam
chmod +x opam
./opam init --comp=4.01.0 #(then say 'y' to the final question)
eval `./opam config env`
./opam install extlib.1.5.4 atdgen.1.6.0 javalib.2.3.1 sawja.1.5.1 #(then say 'y' to the question)
```

If you do not require support for the C/Objective-C analysis in Infer,
and only wish to analyse Java files, continue with these
instructions. By the way, Java 1.8 is not supported.

```bash
cd infer
make -C infer java
export PATH=`pwd`/infer/bin:$PATH
```

To compile support for both Java and C/Objective-C, do this
instead. This assumes that gcc >= 4.7.2 is already installed on your
system. If your distribution is Ubuntu 12.04.4 LTS, you can install
gcc-4.8 and g++-4.8. Follow the following instructions to do that. You
may skip this step in other distributions with a recent enough version
of gcc, for instance Debian 7.

```bash
sudo apt-get install python-software-properties
sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt-get update
sudo apt-get install gcc-4.8 g++-4.8
sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 60 --slave /usr/bin/g++ g++ /usr/bin/g++-4.8
```

Then continue with:

```bash
cd infer
./update-fcp.sh
../facebook-clang-plugin/clang/setup.sh  # go have a coffee :)
./compile-fcp.sh
make -C infer
export PATH=`pwd`/infer/bin:$PATH
```
