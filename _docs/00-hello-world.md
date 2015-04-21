---
id: hello-world
title: Hello World
layout: docs
permalink: /docs/hello-world.html
section: Quick Start
section_order: 00
order: 02
---

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

## Hello World Objective-C

Here is a simple Objective-C example to illustrate Infer at work.

```Objective-C
// Hello.m
#import <Foundation/Foundation.h>

@interface Hello: NSObject
@property NSString* s;
@end

@implementation Hello
NSString* m() {
    Hello* hello = nil;
    return hello->_s;
}
@end
```

To run Infer on the file, do

```bash
$> infer -- clang -c Hello.m
...
Hello.m:10 NULL_DEREFERENCE
  [B1] pointer hello last assigned on line 9 could be null and is dereferenced at line 10, column 12
```

Now edit the file to use the getter instead of accessing the instance variable:

```Objective-C
// Hello.m
#import <Foundation/Foundation.h>

@interface Hello: NSObject
@property NSString* s;
@end

@implementation Hello
NSString* m() {
    Hello* hello = nil;
    return hello.s;
}
@end
```

This time we get no error:

```bash
$> infer -- clang -c Hello.m
...
no errors
```
