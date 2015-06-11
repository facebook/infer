---
id: infer-bug-types
title: Infer bug types
layout: docs
permalink: /docs/infer-bug-types.html
section: Bug Types Reference
section_order: 03
order: 01
---

Here is an overview of the types of bugs currently reported by Infer.

- Bugs reported in Java
  - [Resource leak](/docs/infer-bug-types.html#RESOURCE_LEAK) 
  - [Null dereference](/docs/infer-bug-types.html#NULL_DEREFERENCE)

- Bugs reported in C and Objective-C
  - [Resource leak](/docs/infer-bug-types.html#RESOURCE_LEAK) 
  - [Memory leak](/docs/infer-bug-types.html#MEMORY_LEAK)
  - [Null dereference](/docs/infer-bug-types.html#NULL_DEREFERENCE)
  - [Parameter not null checked](/docs/infer-bug-types.html#PARAMETER_NOT_NULL_CHECKED)
  - [Ivar not null checked](/docs/infer-bug-types.html#IVAR_NOT_NULL_CHECKED)
  - [Premature nil termination argument](/docs/infer-bug-types.html#PREMATURE_NIL_TERMINATION_ARGUMENT)

- Bugs reported only in Objective-C
  - [Retain cycle](/docs/infer-bug-types.html#RETAIN_CYCLE)

## <a name="RESOURCE_LEAK"></a> Resource leak

Infer reports resource leaks in C, Objective-C and Java. In general, resources are entities such as files, sockets, connections, etc, that need to be closed after being used. 

### Resource leak in C

This is an example of a resource leak in C code:

```c
-(void) resource_leak_bug {
    FILE *fp;
    fp=fopen("c:\\test.txt", "r"); // file opened and not closed.
}
```

### Resource leak in Java

For the remaining of this section, we will consider examples of resource leaks in Java code.

TIP: A common source of bugs is  <b>exceptions skipping past close() statements</b>. That is the first thing to look for if INFER reports a potential resource leak.

### Basics and Standard Idiom

Some objects in Java, the <i>resources</i>, are supposed to be closed when you stop using them, and failure to close is a <i>resource leak</i>. Resources include
input streams, output streams, readers, writers, sockets, http connections, cursors, and json parsers.

The standard idiom is

```java
  // Standard idiom
  Allocate resource
  try {
    do some stuff
  } finally {
    close resource
  }
```

or more for example,

```java
  //  Standard Idiom
  public static void foo () throws IOException{
    FileOutputStream fos = new FileOutputStream(new File("whatever.txt"));
    try {
      fos.write(7);
    } finally {
      fos.close();
    }
  }
```
and you should use the standard idiom for the most part, when you don't want to return the resource to the surrounding context.

Sometimes people just leave out close(), and that is a bug, but more typically exceptional paths are the root of the problem, as in

```java
  // leak because of exception
  public static void foo () throws IOException {
    FileOutputStream fos = new FileOutputStream(new File("whatever.txt"));
    fos.write(7);   //DOH! What if exception?
    fos.close();
  }
```

where an exception in fos.write will cause execution to skip past the close() statement.

#### Multiple Resources Bugs

We can deal with multiple resources correctly and simply just by nesting the standard idiom.

```java
  // Two Resources nested
  public static void foo() throws IOException {
    FileInputStream fis = new FileInputStream(new File("whatever.txt"));
    try {
      FileOutputStream fos = new FileOutputStream(new File("everwhat.txt"));
      try {
        fos.write(fis.read());
      } finally {
        fos.close();
      }
    } finally {
      fis.close();
    }
  }
```

Bugs often occur when using multiple resources in other ways because of exceptions in close() methods. For example,

```java
  // Classic Two Resources Bug
  public static void foo() throws IOException {
    FileInputStream fis = null;
    FileOutputStream fos = null;
    try {
      fis = new FileInputStream(new File("whatever.txt"));
      fos = new FileOutputStream(new File("everwhat.txt"));
      fos.write(fis.read());
    } finally {
      if (fis!=null)  fis.close();
      if (fos!=null) fos.close();
    }
  }
```

Here, if there is an exception in the call to fis.close() execution will skip past fos.close(); a leak.

Another way, besides the standard idiom, to deal with this problem is to swallow exceptions.

```java
  // Two Resources Fix 1
  public static void foo() throws IOException {
    FileInputStream fis = null;
    FileOutputStream fos = null;
    try {
      fis = new FileInputStream(new File("whatever.txt"));
      fos = new FileOutputStream(new File("everwhat.txt"));
      fos.write(fis.read());
    } finally {
      try {
        if (fis!=null) fis.close();
      } catch (Exception e) {};  // Exception swallowing
      if (fos!=null) fos.close();
    }
  }
```

You can also swallow the exception on the output stream. Some people prefer not to swallow output stream exceptions, and also flush before closing.
      http://code.google.com/p/guava-libraries/issues/detail?id=1118

Notice that the nested standard idiom does not need the checks for null, which are in there in this case to protect against the case when one of the allocations throws an exception, in which case one would get a NullPointerException.

###  Nested_Allocations

When a resource allocation is included as an argument to a constructor,
if the constructor fails it can leave an an unreachable resource that no one can close.

For example
  gzipOutputStream = new GZIPOutputStream(new FileOutputStream(out));
is bad in case the outer constructor, GZIPOutputStream, throws an exception. In that case, no one will have a hold of the  FileOutputStream and so no one will be able to close it.

In such a case you need to move  the allocation the FileOutputStream out of the nested position and name it, so you are able to close if anything goes wrong during execution of the GZIPOutputStream constructor.

Here are resources that can throw exceptions i their constructor(s).

 - ObjectInputStream , ObjectOutputStream, PipedInputStream, PipedOutputStream, PipedReader, PipedWriter,
    JarInputStream, JarOutputStream, GZIPInputStream, GZIPOutputStream , ZipFile
  all throw IOException
 - PrintStream throws UnsupportedEncodingException

The constructors for FileInputStream, FileOutputStream and RandomAccessFile throw FileNotFoundException, but these cases are not problematic in the sense that their arguments are not resources and so they do not cause the nested resource leak.

###  Allocation of JSonParser and Cursor resources

Some resources are created inside libraries instead of by "new".

Cursor is an interface, the actual resources are something like SQLiteCursor.  So, every time you call a function that returns a Cursor object, there is an allocation.

For instance, in the functions  from SQLiteDatabase
   query(…) and rawQuery(…)
allocate a cursor resource. For
SQLiteQueryBuilder, ContentProviderClient, ContentResolver. MediaStore and DownloadManager it is only
   query(…)
Cursor objects cursor created by these functions need to be closed (i.e., cursor.close()).

Similarly, JsonParser is an abstract class, and create a resource in functions  from the class JsonFactory
  createParser(byte[] data)
  createParser(byte[] data, int offset, int len)
  createParser(String content)
  createParser(URL url)
  createParser(File f)
JsonParser objects js created by these functions need to be closed (jp.close()). On the other hand . JasonParsers
gotten from
  createParser(InputStream in) and
  createParser(Reader r)
give you JsonParsers that don’t need to be closed. This is because they receive the resource from somewhere that will maintain the responsibility to close it.

### Escaping resources and exceptions

Sometimes you want to return a resource to the outside, in which case you should not close it, but you still need to be careful of exceptions in
case control skips past the return leaving no one to close. Here is a simple example of a positive use of escaping resources.

```java
  // An escaping resource, shouldn't close
  public BugReportAttachment createAttachment(File reportDirectory, String fileName)
      throws FileNotFoundException {
    File file = new File(reportDirectory, fileName);
    OutputStream stream = new FileOutputStream(file);
    return new BugReportAttachment(Uri.fromFile(file), stream);
  }
```
In this case it is intended that an object that wraps `stream` is passed to the caller of `createAttachment`.
You should certainly not close stream here, because it is being passed to the outside.

But for escaping resources like this you still need to be careful of exceptions. For example, in

```java
  // An escaping resource, and a leak
  public BugReportAttachment createAttachment(File reportDirectory, String fileName)
      throws FileNotFoundException {
    File file = new File(reportDirectory, fileName);
    OutputStream stream = new FileOutputStream(file);
    stream.write(7);
    return new BugReportAttachment(Uri.fromFile(file), stream);
  }
```

if stream.write(7) throws an exception, then no one will have a hold of stream, and no one will be able to close it; a leak.

###  Java 7's try-with-resources

**(For use only if you have are using Java 7)**

Clearly, accounting for the ramifications of all the exceptional cases is complicated, and there is a better way in Java 7.

```java
  // Two Resources Fix 2; via try-with-resources
  public static void foo() throws IOException {
    try (
      FileInputStream fis = new FileInputStream(new File("whatever.txt"));
      FileOutputStream fos = new FileOutputStream(new File("everwhat.txt"))
    ) {
      fos.write(fis.read());
    }
  }
```

 All the complicated exceptional cases above are (apparently) covered by this construct, and the result is much simpler.

So, if you are trying to fix a potential leak in code with multiples resources you can go ahead and try to understand whether the potential leak is real.
Or, if the code is complex and it is hard to figure out, it would be perfectly legitimate to   simply convert the code over to
try-with-resources if you have access to Java 7, so as to save yourself some brain-cycles. You will also end up with cleaner code.

If try-with-resources is so great you should <i>always</i> use it. But you shouldn't… Try-with-resources gives resources static scoping, and works via a stack discipline. Sometimes, you want a resource to persist beyond scope,
as in the escaping example above.
In an escaping example  maybe you could refactor lots of code so that try-with-resources applies, and maybe you cannot in a sensible way.
This just illustrates that, though you might hear people say that try-with-resources "solves" the resource problem, it does not. It is very useful, but you cannot use it blindly
when you see a resource-allocation site.

## <a name="MEMORY_LEAK"></a>Memory leak


###Memory leak in C

This error type is only reported in C and Objective-C code. In Java we do not report memory leaks because it is a garbage collected language.

In C, Infer reports memory leaks when objects are created with `malloc` and not freed. For example:

```c
-(void) memory_leak_bug { 
    struct Person *p = malloc(sizeof(struct Person));
}
```

### Memory leak in Objective-C

Additionally, in Objective-C, Infer reports memory leaks that happen when objects from Core Foundation or Core Graphics don't get released. 

```objc
-(void) memory_leak_bug_cf { 
    CGPathRef shadowPath = CGPathCreateWithRect(self.inputView.bounds, NULL); //object created and not released.
}
```
## <a name="RETAIN_CYCLE"></a> Retain cycle


A retain cycle is a situation when object A retains object B, and object B retains object A at the same time. Here is an example:

```objc
@class Child;
@interface Parent : NSObject {
    Child *child; // Instance variables are implicitly __strong
}
@end
@interface Child : NSObject {
    Parent *parent;
}
@end
```

You can fix a retain cycle in ARC by using __weak variables or weak properties for your "back links", i.e. links to direct or indirect parents in an object hierarchy:

```objc
@class Child;
@interface Parent : NSObject {
    Child *child;
}
@end
@interface Child : NSObject {
    __weak Parent *parent;
}
@end
```
## <a name="NULL_DEREFERENCE"></a>Null Dereference


Infer reports null dereference bugs in C, Objective-C and Java. The issue is about a pointer that can be `null` 
and it is dereferenced. This leads to a crash in all the above languages.

### Null dereference in C

Here is an example of an inter-procedural null dereference bug in C:

```c
struct Person {
  int age;
  int height;
  int weight;
};
int get_age(struct Person *who) {
  return who->age;
}
int null_pointer_interproc() {
  struct Person *joe = 0;
  return get_age(joe); 
}
```

### Null dereference in Objective-C 

In Objective-C, null dereferences are less common than in Java, but they still happen and their cause can be hidden. 
In general, passing a message to nil does not cause a crash and returns `nil`, but dereferencing a pointer directly 
does cause a crash as well as calling a `nil` block.

```objc
-(void) foo:(void (^)())callback {
    callback();
}

-(void) bar {
    [self foo:nil]; //crash
}
```

Moreover, there are functions from the libraries that do not allow `nil` to 
be passed as argument. Here are some examples:


```objc
-(void) foo {
    NSString *str = nil;
    NSArray *animals = @[@"horse", str, @"dolphin"]; //crash
}

-(void) bar {
  CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB(); //can return NULL
  ...
  CFRelease(colorSpace); //crashes if called with NULL
}
```

### Null dereference in Java

Many of Infer's reports of potential NPE's come from code of the form

```java
  p = foo(); // foo() might return null
  stuff();
  p.goo();   // dereferencing p, potential NPE
```

If you see code of this form, then you have several options.

<b> If you are unsure whether or not foo() will return null </b>, you should ideally
    i. Change the code to ensure that foo() can not return null
    ii. Add a check for whether p is null, and do something other than dereferencing p when it is null.

Sometimes, in case ii it is not obvious what you should do when p is null. One possibility (a last option) is to throw an exception, failing early.
This can be done using checkNotNull as in the following code:

```java
  // code idiom for failing early

  import static com.google.common.base.Preconditions.checkNotNull;

  //... intervening code

  p = checkNotNull(foo()); // foo() might return null
  stuff();
  p.goo();   // dereferencing p, potential NPE
```

The call checkNotNull(foo()) will never return null; in case foo()  returns null it fails early by throwing an NPE.

<b> If you are absolutely sure that foo() will not be null </b>, then if you land your diff this case will no longer be reported after your diff makes it to master.  In the future we might include analysis directives (hey, analyzer, p is not null!) like in Hack that tell the analyzer
the information that you know, but that is for later.

##<a name="PARAMETER_NOT_NULL_CHECKED"></a> Parameter not null checked

This error type is reported only in Objective-C. It is similar to Null dereference, but Infer hasn't found a whole trace where the error can happen, but only found that a null dereference can happen if you call a method with nil as an argument. Therefore it is only a warning. For example:

```objc
  -(int) foo:(A* a) {
      B b* = [a foo]; // sending a message with receiver nil returns nil
      return b->x; // dereferencing b, potential NPE if you pass nil as the argument a.
  }
```

or when the parameter is a block:

```objc
   -(void) foo:(void (^)(BOOL))block {
      block(YES); // calling a nil block will cause a crash.
   }
```

Possible solutions are adding a check for `nil`, or making sure that the method is not called with `nil`.

## <a name="IVAR_NOT_NULL_CHECKED"></a> Ivar not null checked

This error type is only reported in Objective-C. This is similar to Null dereference, but Infer hasn't found a whole trace where the error can happen, but only found that a null dereference can happen if an instance variable of a parameter is `nil`. For example:

```objc
  -(int) foo {
      B b* = [self->_a foo]; // sending a message with receiver nil returns nil
      return b->x; // dereferencing b, potential NPE if you pass nil as the argument a.
  }
```

Possible solutions are adding a check for `nil`, or making sure that the method is not called with `nil`.

##<a name="PREMATURE_NIL_TERMINATION_ARGUMENT"></a> Premature nil termination argument

This error type is reported in C and Objective-C. In many variadic methods, `nil` is used to signify the end of the list of input objects. This is similar to nil-termination of C strings. If one of the arguments that is not the last argument to the method is `nil` as well, Infer reports an error because that may lead to unexpected behavior.

An example of such variadic methods is [arrayWithObjects](https://developer.apple.com/library/prerelease/ios/documentation/Cocoa/Reference/Foundation/Classes/NSArray_Class/index.html#//apple_ref/occ/clm/NSArray/arrayWithObjects)

```objc
  NSArray *foo = [NSArray arrayWithObjects: @"aaa", str, @"bbb", nil];
```

In this example, if `str` is `nil` then an array `@[@"aaa"]` of size 1 will be created, and not an array `@[@"aaa", str, @"bbb"]` of size 3 as expected.

