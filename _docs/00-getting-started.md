---
id: getting-started
title: Getting started with Infer
layout: docs
permalink: /docs/getting-started.html
section: Quick Start
section_order: 00
order: 01
---
## Dependencies

To run Infer you will need Python 2.7

## Getting Infer

We provide pre-built binaries for Infer. Currently, only Mac OS X and
Linux 64 bits are supported.

- Mac OS X: [https://github.com/facebook/infer/releases/download/v0.2.0/infer-osx-v0.2.0.tar.xz](https://github.com/facebook/infer/releases/download/v0.2.0/infer-osx-v0.2.0.tar.xz)
- Linux: [https://github.com/facebook/infer/releases/download/v0.2.0/infer-linux64-v0.2.0.tar.xz](https://github.com/facebook/infer/releases/download/v0.2.0/infer-linux64-v0.2.0.tar.xz)


## Installing Infer

Open a terminal and go to the directory where you downloaded
Infer. Unpack the tarball:

- If you are on Mac OS X, run

    ```bash
    tar xf infer-osx-v0.2.0.tar.xz
    ```

- If you are on Linux, run

    ```bash
    tar xf infer-linux64-v0.2.0.tar.xz
    ```

This will create an ```infer-osx-v0.2.0/``` directory (or an
```infer-linux64-v0.2.0/``` directory), with Infer's main executables
located in ```infer-osx-v0.2.0/infer/infer/bin/```.

### Adding Infer to your PATH

We recommend adding Infer to your PATH to make it easier to use from
anywhere in the system. Alternatively, you can use the full path to
the infer executable to invoke it. In our documentation, we will
assume that Infer's binaries are in your PATH.

If you are using bash, you can run the following command from the
directory where you unpacked the tarball to add Infer to your PATH.

```bash
cd infer-*v0.2.0 &&
echo "export PATH=\"\$PATH:`pwd`/infer/infer/bin\"" \ >> ~/.bash_profile &&
source ~/.bash_profile
```

You can find out which shell you are using by running ```echo
$SHELL``` in your terminal. Adapt the command above to your specific
shell as needed. If you are running bash on Linux, you may want to
replace "~/.bash\_profile" by "~/.bashrc" in the command above if
"~/.bash\_profile" does not exist.
