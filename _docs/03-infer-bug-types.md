---
id: infer-bug-types
title: Infer Bug Types
layout: docs
permalink: /docs/infer-bug-types.html
section: Bug Types Reference
section_order: 01
order: 00
---

## RESOURCE\_LEAK

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

###  Multiple Resources Bugs

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
is bad in case the outer constructor, GZIPOutputStream, throws an exception. In that case, no one will have ahold of the  FileOutputStream and so no one will be able to close it.

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

###  Escaping resources and exceptions

Sometimes you want to return a resource to the outside, in which case you should not close it, but you still need to be careful of exceptions in
axe control skips past the return leaving no one to close. Here is a simple example from the fbandroid codebase of a positive use of escaping resources.

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

if stream.write(7) throws an exception, then no one will have ahold of stream, and no one will be able to close it; a leak.

###  Java 7's try-with-resources

<b> (For use only if you have are using Java 7: e.g., yes in Buck, no in fbandroid.)</b>

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

## NULL\_DEREFERENCE

Many of Infer's reports of potential NPE's come from code of the form

```java
  p = foo(); // foo() might return null
  stuff();
  p.goo();   // dereferencing p, potential NPE
```

If you see code of this form, then you have several options.

<b> If you are unsure whether or not foo() will return null </b>, you should ideally
    i. Change the code to ensure that foo() can not return null   (e.g., diff https://phabricator.fb.com/D1170449)
    ii. Add a check for whether p is null, and do something other than dereferencing p when it is null.(e.g., diff  https://phabricator.fb.com/D861920)

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

<b> If you are absolutely sure that foo() will not be null </b>, then if you land your diff this case will no longer be reported after your diff makes it to master.  In the future we might include analysis directives (hey, analyser, p is not null!) like in Hack that tell the analyser
the information that you know, but that is for later.

## PARAMETER\_NOT\_NULLABLE

This is similar to NULL_DEREFERENCE, but Infer hasn't found a whole trace where the error can happen, but only found that a null dereference can happen if you call a method with nil as an argument. For example:

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

## IVAR\_NOT\_NULLABLE

This is similar to NULL_DEREFERENCE, but Infer hasn't found a whole trace where the error can happen, but only found that a null dereference can happen if an instance variable of a parameter is nil. For example:

```objc
  -(int) foo {
      B b* = [self->_a foo]; // sending a message with receiver nil returns nil
      return b->x; // dereferencing b, potential NPE if you pass nil as the argument a.
  }
```

## PREMATURE\_NIL\_TERMINATION\_ARGUMENT

In many variadic methods, `nil` is used to signify the end of the list of input objects. This is similar to nil-termination of C strings. If one of the arguments that is not the last argument to the method is nil as well, Infer reports an error because that may lead to unexpected behaviour.

An example of such variadic methods is [arrayWithObjects](https://developer.apple.com/library/prerelease/ios/documentation/Cocoa/Reference/Foundation/Classes/NSArray_Class/index.html#//apple_ref/occ/clm/NSArray/arrayWithObjects)

```objc
  NSArray *foo = [NSArray arrayWithObjects: @"aaa", str, @"bbb", nil];
```

In this example, if `str` is `nil` then an array `@[@"aaa"]` of size 1 will be created, and not an array `@[@"aaa", str, @"bbb"]` of size 3 as expected.


## CHECKERS\_IMMUTABLE\_CAST

This error fires when an immutable collection is returned from a method whose type is mutable.

```java
  public List<String> getSomeList() {
    ImmutableList<String> l = foo(...);
    return l;
  }
```

This can lead to a runtime error if users of ` getSomeList` try to modify the list e.g. by adding elements.

'''Action:''' you can change the return type to be immutable, or make a copy of the collection so that it can be modified.

## CHECKERS\_PRINTF\_ARGS

This error fires when either the arguments of a `printf` like function don't match the given format string. With `BLog` another common mistake is passing an exception as the last argument because this is how android's `Log` are used. Note that this argument will simply be swallowed. The exception should be passed ''before'' the format string.

```java
catch (Execption ex) {
  // DO
  BLog.e(TAG, ex, "Some %s", "format");
  // DON'T
  BLog.e(TAG, "Some %s", "format", ex); // ex is swallowed
}
```java
