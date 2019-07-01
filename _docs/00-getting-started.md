---
docid: getting-started
title: Getting started with Infer
layout: docs
permalink: /docs/getting-started.html
---

## How to Download and Install Infer

You can either install via Homebrew (Mac only), download our binary releases,
build infer from source, or use our Docker image. As of April 2019 (version 0.16.0),
**the binary releases will require about 1.2 GB of disk space.** Downloading the source
code and building it will require a lot more than 1.2 GB, to compile clang for instance.

### Use Homebrew (macOS only)

On macOS, the simplest way is to use [Homebrew](http://brew.sh/). Type this into a terminal:

```sh
brew install infer
```

### Use a Direct Download from Github (Linux or macOS)

On Linux, or if you do not wish to use Homebrew on Mac, you can directly
download our [latest binary
release](https://github.com/facebook/infer/releases/latest). Download
the tarball, then extract it anywhere on your system to start using infer.

For example, use this to download the **Linux version** of infer to `/opt`. You
should replace `VERSION` with the latest release, e.g. `VERSION=0.16.0`:

```sh
VERSION=0.XX.Y; \
curl -sSL "https://github.com/facebook/infer/releases/download/v$VERSION/infer-linux64-v$VERSION.tar.xz" \
| sudo tar -C /opt -xJ && \
ln -s "/opt/infer-linux64-v$VERSION/bin/infer" /usr/local/bin/infer
```

Use this to download the **Mac version** of infer to `/Applications` and optionally put
it on the `PATH`. You should replace `0.16.0` with the [latest release version](https://github.com/facebook/infer/releases/latest):

```sh
curl -O -L "https://github.com/facebook/infer/releases/download/v0.16.0/infer-osx-v0.16.0.tar.xz"
shasum infer-osx-v0.16.0.tar.xz      # Validate the SHA checksum, to ensure it's the correct file
tar -xJf infer-osx-v0.16.0.tar.xz    # Unzip the compressed file. This will create a new folder
mv infer-osx-v0.16.0/ /Applications/ # Move the infer folder into /Applications
rm infer-osx-v0.16.0.tar.xz          # Delete the original downloaded file

# These next steps are optional, to add the infer binary to the PATH environment variable

# Open ~/.bash_profile in a text editor
nano ~/.bash_profile

# Add this line to the end of ".bash_profile". Then save the file with Ctrl+X, then Y, then Enter
PATH=/Applications/infer-osx-v0.16.0/bin:$PATH

# Alternatively, create a symbolic link to infer (instead of editing '.bash_profile')
ln -s "/Applications/infer-osx-v0.16.0/bin/infer" /usr/local/bin/infer

# Check that infer is on the PATH:
type infer
```

If the binaries do not work for you, or if you would rather build
infer from source, follow the [install from
source](https://github.com/facebook/infer/blob/master/INSTALL.md#install-infer-from-source)
instructions to install Infer on your system.

Alternatively, use our
[Docker](https://docs.docker.com/engine/installation/) image:

```sh
wget -O Dockerfile https://raw.githubusercontent.com/facebook/infer/master/docker/0.14.0/Dockerfile
wget -O run.sh https://raw.githubusercontent.com/facebook/infer/master/docker/0.14.0/run.sh
sh run.sh
```

## Try Infer in your browser

Try Infer on a small example on [Codeboard](https://codeboard.io/projects/11587?view=2.1-21.0-22.0).

# Next Sections

[//]: # (These links are required because they are completely hidden and inaccessible when the web page is viewed on a tablet or phone)

* [Analyzing Apps or Projects: make, Ant, gradle, etc](https://fbinfer.com/docs/analyzing-apps-or-projects.html)
* [Infer Workflow](https://fbinfer.com/docs/infer-workflow.html)
* [Advanced Usage](https://fbinfer.com/docs/advanced-features.html)
* [Infer Manuals](https://fbinfer.com/docs/man-pages.html)

