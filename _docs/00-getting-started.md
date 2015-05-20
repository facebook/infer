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

We provide pre-built binaries for Infer, depending on your operating system.

### Mac OS X
[http://fb-infer.org/downloads/fb-infer-osx-latest.pkg](http://home.fburl.com/~cristianoc/fb-infer-osx-latest.pkg)

Install the package and update `$PATH` by typing the following command in your terminal:

```bash
echo "export PATH=$PATH:/usr/local/infer/bin" \
     >> ~/.bashrc && source ~/.bashrc
```

If there is a security warning that prevents you from installing Infer, open the folder with the pkg file in Finder, then right click on it and select Open.

### Linux (64 bit)

[http://home.fburl.com/~martinoluca/infer-linux-x64.tar.gz](http://home.fburl.com/~martinoluca/infer-linux-x64.tar.gz)

Run the commands below in your terminal to download and install Infer.

 ```bash
wget http://home.fburl.com/~martinoluca/infer-0.1-x64-linux.tar.xz
tar xf infer-0.1-x64-linux.tar.xz
# this assumes you use bash, adapt to your needs in case you use
# another shell
echo "export PATH=$PATH:`pwd`/infer-0.1-x64-linux/infer/infer/bin" \
     >> ~/.bashrc && source ~/.bashrc
```
