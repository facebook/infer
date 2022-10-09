Infer reports resource leaks in C, Objective-C and Java. In general, resources
are entities such as files, sockets, connections, etc, that need to be closed
after being used.

### Resource leak in C

This is an example of a resource leak in C code:

```c
-(void) resource_leak_bug {
    FILE *fp;
    fp=fopen("c:\\test.txt", "r"); // file opened and not closed.
}
```

### Resource leak in Java

For the remaining of this section, we will consider examples of resource leaks
in Java code.

TIP: A common source of bugs is <b>exceptions skipping past close()
statements</b>. That is the first thing to look for if INFER reports a potential
resource leak.

### Basics and Standard Idiom

Some objects in Java, the <i>resources</i>, are supposed to be closed when you
stop using them, and failure to close is a <i>resource leak</i>. Resources
include input streams, output streams, readers, writers, sockets, http
connections, cursors, and json parsers.

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

and you should use the standard idiom for the most part, when you don't want to
return the resource to the surrounding context.

Sometimes people just leave out close(), and that is a bug, but more typically
exceptional paths are the root of the problem, as in

```java
  // leak because of exception
  public static void foo () throws IOException {
    FileOutputStream fos = new FileOutputStream(new File("whatever.txt"));
    fos.write(7);   //DOH! What if exception?
    fos.close();
  }
```

where an exception in fos.write will cause execution to skip past the close()
statement.

#### Multiple Resources Bugs

We can deal with multiple resources correctly and simply just by nesting the
standard idiom.

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

Bugs often occur when using multiple resources in other ways because of
exceptions in close() methods. For example,

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

Here, if there is an exception in the call to fis.close() execution will skip
past fos.close(); a leak.

Another way, besides the standard idiom, to deal with this problem is to swallow
exceptions.

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

You can also swallow the exception on the output stream. Some people prefer not
to swallow output stream exceptions, and also flush before closing.
http://code.google.com/p/guava-libraries/issues/detail?id=1118

Notice that the nested standard idiom does not need the checks for null, which
are in there in this case to protect against the case when one of the
allocations throws an exception, in which case one would get a
NullPointerException.

### Nested_Allocations

When a resource allocation is included as an argument to a constructor, if the
constructor fails it can leave an unreachable resource that no one can close.

For example gzipOutputStream = new GZIPOutputStream(new FileOutputStream(out));
is bad in case the outer constructor, GZIPOutputStream, throws an exception. In
that case, no one will have a hold of the FileOutputStream and so no one will be
able to close it.

In such a case you need to move the allocation the FileOutputStream out of the
nested position and name it, so you are able to close if anything goes wrong
during execution of the GZIPOutputStream constructor.

Here are resources that can throw exceptions i their constructor(s).

- ObjectInputStream , ObjectOutputStream, PipedInputStream, PipedOutputStream,
  PipedReader, PipedWriter, JarInputStream, JarOutputStream, GZIPInputStream,
  GZIPOutputStream , ZipFile all throw IOException
- PrintStream throws UnsupportedEncodingException

The constructors for FileInputStream, FileOutputStream and RandomAccessFile
throw FileNotFoundException, but these cases are not problematic in the sense
that their arguments are not resources and so they do not cause the nested
resource leak.

### Allocation of JSonParser and Cursor resources

Some resources are created inside libraries instead of by "new".

Cursor is an interface, the actual resources are something like SQLiteCursor.
So, every time you call a function that returns a Cursor object, there is an
allocation.

For instance, in the functions from SQLiteDatabase query(…) and rawQuery(…)
allocate a cursor resource. For SQLiteQueryBuilder, ContentProviderClient,
ContentResolver. MediaStore and DownloadManager it is only query(…) Cursor
objects cursor created by these functions need to be closed (i.e.,
cursor.close()).

Similarly, JsonParser is an abstract class, and create a resource in functions
from the class JsonFactory createParser(byte[] data) createParser(byte[] data,
int offset, int len) createParser(String content) createParser(URL url)
createParser(File f) JsonParser objects js created by these functions need to be
closed (jp.close()). On the other hand . JasonParsers gotten from
createParser(InputStream in) and createParser(Reader r) give you JsonParsers
that don’t need to be closed. This is because they receive the resource from
somewhere that will maintain the responsibility to close it.

### Escaping resources and exceptions

Sometimes you want to return a resource to the outside, in which case you should
not close it, but you still need to be careful of exceptions in case control
skips past the return leaving no one to close. Here is a simple example of a
positive use of escaping resources.

```java
  // An escaping resource, shouldn't close
  public BugReportAttachment createAttachment(File reportDirectory, String fileName)
      throws FileNotFoundException {
    File file = new File(reportDirectory, fileName);
    OutputStream stream = new FileOutputStream(file);
    return new BugReportAttachment(Uri.fromFile(file), stream);
  }
```

In this case it is intended that an object that wraps `stream` is passed to the
caller of `createAttachment`. You should certainly not close stream here,
because it is being passed to the outside.

But for escaping resources like this you still need to be careful of exceptions.
For example, in

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

if stream.write(7) throws an exception, then no one will have a hold of stream,
and no one will be able to close it; a leak.

### Java 7's try-with-resources

**(For use with Java 7 only)**

Clearly, accounting for the ramifications of all the exceptional cases is
complicated, and there is a better way in Java 7.

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

All the complicated exceptional cases above are (apparently) covered by this
construct, and the result is much simpler.

So, if you are trying to fix a potential leak in code with multiples resources
you can go ahead and try to understand whether the potential leak is real. Or,
if the code is complex and it is hard to figure out, it would be perfectly
legitimate to simply convert the code over to try-with-resources if you have
access to Java 7, so as to save yourself some brain-cycles. You will also end up
with cleaner code.

If try-with-resources is so great you should <i>always</i> use it. But you
shouldn't… Try-with-resources gives resources static scoping, and works via a
stack discipline. Sometimes, you want a resource to persist beyond scope, as in
the escaping example above. In an escaping example maybe you could refactor lots
of code so that try-with-resources applies, and maybe you cannot in a sensible
way. This just illustrates that, though you might hear people say that
try-with-resources "solves" the resource problem, it does not. It is very
useful, but you cannot use it blindly when you see a resource-allocation site.
