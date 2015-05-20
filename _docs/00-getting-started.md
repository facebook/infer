---
id: getting-started
title: Getting started with Infer
layout: docs
permalink: /docs/getting-started.html
section: Quick Start
section_order: 00
order: 01
---

## Installing Infer

We provide pre-built binaries for Infer, depending on your operating system:

### Mac OS X
[http://fb-infer.org/downloads/fb-infer-osx-latest.pkg](http://home.fburl.com/~cristianoc/fb-infer-osx-latest.pkg)

Install the package and update the path (if there is a security warning, open it in Finder, right click and select Open).

```bash
 $> echo "export PATH=$PATH:/usr/local/infer/bin" >> ~/.bashrc && source ~/.bashrc
```

### Linux (64 bit)

 [http://home.fburl.com/~martinoluca/infer-linux-x64.tar.gz](http://home.fburl.com/~martinoluca/infer-linux-x64.tar.gz)

 ```bash
 $> tar xzvf infer_linux_x64.tar.gx -C ${HOME}
 ```

```bash
 $> echo "export PATH=$PATH:${HOME}/infer/infer/bin" >> ~/.bashrc && source ~/.bashrc
```
