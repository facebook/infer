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

## Hello World Java

Here is a simple java example to illustrate Infer at work.

```java
// Hello.java
class Hello {
  int test() {
    String s = null;
    return s.length();
  }
}
```

To run Infer on the file, do

```bash
$> infer -- javac Hello.java
...
/Users/me/test/Hello.java:5: error: NULL_DEREFERENCE
  [B1] object s last assigned on line 4 could be null and is dereferenced at line 5  
```

Now edit the file to add null checks:

```java
// Hello.java
class Hello {
  int test() {
    String s = null;
    return s == null ? 0 : s.length();
  }
}
```
  
This time we get no error:

```bash
$> infer -- javac Hello.java
...
no errors
```

## Hello World C

Here is a simple C example to illustrate Infer at work.

```c
// hello.c
#include <stdlib.h>
int test() {
  int *s = NULL;
  return *s;
}
```

To run Infer on the file, do

```bash
$> infer -- gcc -c hello.c
...
/Users/me/test/hello.c:5: error: NULL_DEREFERENCE
  [B1] pointer s last assigned on line 4 could be null and is dereferenced at line 5, column 10
```

Now edit the file to add null checks:

```c
// hello.c
#include <stdlib.h>
int test() {
  int *s = NULL;
  return s == NULL ? 0 : *s;
}
```
  
This time we get no error:

```bash
$> infer -- gcc -c hello.c
...
no errors
```
