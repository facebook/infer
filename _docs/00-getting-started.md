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
<p>Install the package

[http://fb-infer.org/downloads/fb-infer-osx-latest.pkg](http://home.fburl.com/~cristianoc/fb-infer-osx-latest.pkg)

If you get a security warning that stops you on this step, you can open the
folder holding the pkg file in Finder, then control-click on it and select Open. (Shortly, we will put
a link to a binary here if you prefer not to use a pkg)

Next, update your
`$PATH` with `/usr/local/infer/bin`, e.g. by typing the following command in your terminal:

```bash
echo "export PATH=/usr/local/infer/bin:$PATH" \
     >> ~/.bashrc && source ~/.bashrc
```

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
