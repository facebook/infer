---
docid: getting-started
title: Getting started with Infer
layout: docs
permalink: /docs/getting-started.html
---

## Install Infer on Mac

You'll need [Homebrew](http://brew.sh/). Simply type this into a terminal:

```sh
brew install infer
```

If you get the following error...

```sh
Error: No available formula for infer 
Searching taps...
homebrew/science/infernal
```
... Then you must run this command first:

```sh
brew update
```
And wait a few minutes, and then run `brew install infer`. More info [here](https://github.com/facebook/infer/issues/36).

## Install Infer on Linux

Follow the [install from source](docs/getting-started.html#install-from-source) instructions below to install Infer on your system. Alternatively, use our [Docker](https://docs.docker.com/engine/installation/) image:

```sh
wget -O Dockerfile https://raw.githubusercontent.com/facebook/infer/master/docker/Dockerfile
wget -O run.sh https://raw.githubusercontent.com/facebook/infer/master/docker/run.sh
sudo sh run.sh
```

## Try Infer in your browser

Try Infer on a small example on [Codeboard](https://codeboard.io/projects/11587?view=2.1-21.0-22.0).

## Install from source

We recommend that you start from our latest [GitHub release](https://github.com/facebook/infer/releases/latest), which contains a pre-compiled version of clang, used for our C/Objective-C analyzers.

You'll need to first install the [build dependencies](https://github.com/facebook/infer/blob/master/INSTALL.md#pre-compiled-versions), then run the following commands:

```sh
tar xf infer-<release>.tar.xz
cd infer-<release>/
./build-infer.sh
make install
```
