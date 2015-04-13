---
id: getting-started
title: Getting started with Infer
layout: docs
permalink: /docs/getting-started.html
prev: about-infer.html
next: analyzing-android-app.html
---

## Installing Infer

We provide pre-built binaries for Infer, depending on your operating system:

### Mac OS X
[http://fb-infer.org/downloads/fb-infer-osx-latest.pkg](/downloads/fb-infer-osx-latest.pkg)

Install the package and update the path (if there is a security warning, open it in Finder, right click and select Open).

```bash
$> echo "PATH=\"\$PATH:/usr/local/infer/bin/\"" >> ~/.bashrc && source ~/.bashrc
```

### Linux (64 bit)

 [http://flowtype.org/downloads/infer-linux64-latest.zip](/downloads/flow-linux64-latest.zip)

 ```bash
 $> unzip infer-linux64-latest.zip
 ```

```bash
$> echo "PATH=\"\$PATH:/usr/local/infer/bin/\"" >> ~/.bashrc && source ~/.bashrc
```
