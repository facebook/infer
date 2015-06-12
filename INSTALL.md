#Install Infer

We provide pre-built Infer binaries for Linux and MacOS.  If you just
wish to use Infer, and are not interested in making contributions to
it, then these binaries are all you need.  Otherwise, if you wish to
compile Infer, here are also instructions to do so, depending on your
operating system.

- [Install the Infer binaries](INSTALL.md#install-the-infer-binaries)
	- [Mac OS X](INSTALL.md#mac-os-x)
	- [Linux](INSTALL.md#linux-64-bit)
- [Install Infer from source](INSTALL.md#install-infer-from-source)
	- [Download the Infer repository](INSTALL.md#download-the-infer-repository)
	- [Mac OS X](INSTALL.md#macos-x)
	- [Linux](INSTALL.md#linux)

##Install the Infer binaries

###Requirements

- Python >= 2.7

###Mac OS X 

Get the latest `infer-osx-vXX.tar.xz` from [infer
releases](https://github.com/facebook/infer/releases) and run the
commands below in your terminal to install Infer.

 ```bash
tar xf infer-osx-vXX.tar.xz
# this assumes you use bash, adapt to your needs in case you use
# another shell
echo "export PATH=$PATH:`pwd`/infer-osx/infer/infer/bin" \
     >> ~/.bashrc && source ~/.bashrc
```

###Linux (64 bit)

Get the latest `infer-linux64-vXX.tar.xz` from [infer
releases](https://github.com/facebook/infer/releases) and run the
commands below in your terminal to install Infer.

 ```bash
tar xf infer-linux64-vXX.tar.xz 
# this assumes you use bash, adapt to your needs in case you use
# another shell
echo "export PATH=$PATH:`pwd`/infer-0.1-x64-linux/infer/infer/bin" \
     >> ~/.bashrc && source ~/.bashrc
```


##Install Infer from source

The following instructions describe how to compile Infer on different
platforms.

###Requirements

- Python >= 2.7

### Download the Infer repository

```bash
git clone https://github.com/facebook/infer.git
```

To analyse C and ObjC, Infer requires clang and the
[facebook-clang-plugin](https://github.com/facebook/facebook-clang-plugins). If
you wish to analyse only Java/Android code, then you could skip these
dependencies. Details below.

###MacOS X 

####Requirements

- [opam](https://opam.ocaml.org/doc/Install.html#OSX)

##### Requirements for Java analysis

- Java <= 1.7
- Android dev environment setup for analysis of Android apps.

##### Requirements for C/ObjC analysis 

- XCode <= 6.3, >= 6.1
- clang (in XCode command line tools. You can install them with the command `xcode-select --install`)

###Installation instructions

Install the OCaml dependencies:

```bash
opam init --comp=4.01.0  # (answer 'y' to the question)
opam install sawja.1.5 atdgen.1.5.0 javalib.2.3 extlib.1.5.4
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
./update-fcp.sh
$(cat .facebook-clang-plugin-dir)/facebook-clang-plugin/clang/setup.sh  # go have a coffee :)
./compile-fcp.sh
make -C infer
export PATH=`pwd`/infer/bin:$PATH
```

###Linux

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
./opam install sawja.1.5 atdgen.1.5.0 javalib.2.3 extlib.1.5.4 #(then say 'y' to the question)
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
$(cat .facebook-clang-plugin-dir)/facebook-clang-plugin/clang/setup.sh  # go have a coffee :)
./compile-fcp.sh
make -C infer
export PATH=`pwd`/infer/bin:$PATH
```
