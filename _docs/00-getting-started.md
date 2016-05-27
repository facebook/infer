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

## Install Infer on Linux

The easiest way is via [Docker](https://docs.docker.com/engine/installation/):

```sh
wget -O Dockerfile https://raw.githubusercontent.com/facebook/infer/master/docker/Dockerfile
wget -O run.sh https://raw.githubusercontent.com/facebook/infer/master/docker/run.sh
sh run.sh
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
export PATH=`pwd`/infer/bin:$PATH
```
