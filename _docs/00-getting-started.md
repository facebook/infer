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

To use infer, first you need to [install the dependencies](https://github.com/facebook/infer/blob/master/INSTALL.md#install-the-dependencies).

## Get the latest Infer release

We provide a source release of Infer packaged with pre-build binaries for clang and facebook-clang-plugins for Linux and MacOS. Download the latest release from GitHub [here](https://github.com/facebook/infer/releases/latest/).

## Compile and install Infer

```bash
tar xf infer-*-v0.3.0.tar.xz
cd infer-*-v0.3.0
make -C infer
export PATH=`pwd`/infer/bin:$PATH
```
