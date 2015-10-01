
We provide a source release of Infer packaged with pre-build binaries for clang and facebook-clang-plugins for Linux and MacOS. We encourage you to use this release.
Install the dependencies for your operating system and then follow the instructions
in [Infer's getting-started page](http://fbinfer.com/docs/getting-started.html).


# Install the dependencies

## MacOS X

Here are the prerequisites to be able to compile Infer. Make sure you have
the dependencies installed and opam configured properly. After that either
[use the release](http://fbinfer.com/docs/getting-started.html) (faster), or [compile everything from source](#compile-clang-and-infer-from-source).

### Requirements

- Python 2.7
- opam (instructions [here](https://opam.ocaml.org/doc/Install.html#OSX))
- Java <= 1.7 (only needed for the Java analysis)
- Xcode <= 7.0, >= 6.1 (only needed for the C/Objective-C analysis)
- clang in Xcode command line tools. You can install them with the command `xcode-select --install` (only needed for the C/Objective-C analysis)

### Setup Opam

```bash
opam init -y --comp=4.01.0
eval `opam config env`
opam install -y extlib.1.5.4 atdgen.1.6.0 javalib.2.3.1 sawja.1.5.1
```

## Linux
Here are the prerequisites to be able to compile Infer. Make sure you have
the dependencies installed and opam configured properly. After that either
[use the release](http://fbinfer.com/docs/getting-started.html) (faster), or [compile everything from source](#compile-clang-and-infer-from-source).

### Requirements

- Python 2.7
- opam
- Java <= 1.7 (only needed for the Java analysis)
- gcc >= 4.7.2

### How to install the requirements on Ubuntu 12.04.4 LTS

```bash
sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install git openjdk-7-jdk m4 zlib1g-dev python-software-properties build-essential libgmp-dev libmpfr-dev libmpc-dev unzip
sudo apt-get install gcc-4.8 g++-4.8
sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 60 --slave /usr/bin/g++ g++ /usr/bin/g++-4.8
```

### How to install the requirements on Debian 7 / Ubuntu 14.04

```bash
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install git openjdk-7-jdk m4 zlib1g-dev python-software-properties build-essential libgmp-dev libmpfr-dev libmpc-dev unzip
```

### Setup Opam

```bash
wget https://github.com/ocaml/opam/releases/download/1.2.2/opam-1.2.2-x86_64-Linux -O opam
chmod +x opam
./opam init -y --comp=4.01.0
eval `./opam config env`
./opam install -y extlib.1.5.4 atdgen.1.6.0 javalib.2.3.1 sawja.1.5.1
```

# Compile clang and Infer from source

Infer uses a special version of clang along with a clang plugin. Follow these instructions to compile them from source. Alternatively, you can skip these instructions and use the [release](http://fbinfer.com/docs/getting-started.html).


```bash
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
export PATH=`pwd`/infer/bin:$PATH
```
