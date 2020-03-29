---
id: checkers-bug-types
title: Checkers bug types
---

Here is an overview of the types of bugs currently reported by Infer checkers.

## Captured Strong Self

This will happen in one of two cases generally:

1. One uses `weakSelf` but forgot to declare it weak first.
2. One is using `strongSelf`, declared in a block, in another (inside) block.
   This changes the delicate balance of the `weakSelf`/`strongSelf` use in the
   first block. The retain cycle is avoided there because `strongSelf` is a
   local variable to the block. If `strongSelf` is used in the inside block,
   then it's not a local variable anymore, but a captured variable.

## Checkers immutable cast

This error type is reported in Java. It fires when an immutable collection is
returned from a method whose type is mutable.

```java
  public List<String> getSomeList() {
    ImmutableList<String> l = foo(...);
    return l;
  }
```

This can lead to a runtime error if users of `getSomeList` try to modify the
list e.g. by adding elements.

Action: you can change the return type to be immutable, or make a copy of the
collection so that it can be modified.

## Deadlock

This error is currently reported in Java. A deadlock occurs when two distinct
threads try to acquire two locks in reverse orders. The following code
illustrates a textbook example. Of course, in real deadlocks, the lock
acquisitions may be separated by deeply nested call chains.

```java
  public void lockAThenB() {
    synchronized(lockA) {
      synchronized(lockB) {
       // do something with both resources
      }
    }
  }

  public void lockBThenA() {
    synchronized(lockB) {
      synchronized(lockA) {
       // do something with both resources
      }
    }
  }
```

The standard solution to a deadlock is to fix an order of lock acquisition and
adhere to that order in all cases. Another solution may be to shrink the
critical sections (i.e., the code executing under lock) to the minimum required.

Old-style containers such as `Vector` are synchronized on the object monitor,
which means that deadlocks can occur even without explicit synchronisation on
both threads. For instance:

```java
  public void lockAThenAddToVector() {
    synchronized(lockA) {
      vector.add(object);
    }
  }

  public void lockVectorThenA() {
    synchronized(vector) {
      synchronized(lockA) {
       // do something with both resources
      }
    }
  }
```

Infer has support for detecting these deadlocks too.

To suppress reports of deadlocks in a method `m()` use the
`@SuppressLint("DEADLOCK")` annotation, as follows:

```java
  import android.annotation.SuppressLint;

  @SuppressLint("DEADLOCK")
  public void m() {
  ...
  }
```

## Dead store

This error is reported in C++. It fires when the value assigned to a variables
is never used (e.g., `int i = 1; i = 2; return i;`).

## Empty vector access

This error type is reported only in C++, in versions >= C++11.

The code is trying to access an element of a vector that Infer believes to be
empty. Such an access will cause undefined behavior at runtime.

```c++
#include <vector>
int foo(){
  const std::vector<int> vec;
  return vec[0]; // Empty vector access reported here
}
```

## Field should be nullable

This error type is reported in Java. It fires when a field is not marked
`@Nullable`, but it is

- Nullified in a method

```java
  private List<String> idList;
  public void reset() {
    idList = null;
    ...
  }
```

- Or tested for `null` in a method

```java
  private List<String> idList;
  public void doSomethingWithIdList() {
    if (idList == null) { ... }
  }
```

Action:

- You may want to add `@Nullable` annotation in the field declaration. This will
  inform Infer that the field is intended to be set to `null` at some point. For
  such fields, Infer will emit a warning if you forget to check for `null`
  before accessing them.

```java
  import javax.annotation.Nullable;
  ...
  private @Nullable List<String> idList;
  public void doSomethingWithIdList() {
    int numIds = idList.size();  // Infer will complain that idList is not null-checked here
    ...
  }
```

- If the field is never intended to be nullable, please refactor your codes so
  that it will never be assigned or compared with `null`.

```java
  private List<String> idList = new List<String>();
  ...
```

## Fragment retains view

This error type is Android-specific. It fires when a `Fragment` type fails to
nullify one or more of its declared `View` fields in `onDestroyView`. In
performance-sensitive applications, a `Fragment` should initialize all `View`'s
in `onCreateView` and nullify them in `onDestroyView`. If a `Fragment` is placed
on the back stack and fails to nullify a `View` in `onDestroyView`, it will
retain a useless reference to that `View` that will not be cleaned up until the
`Fragment` is resumed or destroyed.

Action: Nullify the `View` in question in `onDestroyView`.

## Interface not thread-safe

This error indicates that you have invoked an interface method not annotated
with `@ThreadSafe` from a thread-safe context (e.g., code that uses locks or is
marked `@ThreadSafe`). The fix is to add the `@ThreadSafe` annotation to the
interface or to the interface method. For background on why these annotations
are needed, see the detailed explanation
[here](racerd#interface-not-thread-safe).

## Ivar not null checked

This error type is only reported in Objective-C. This is similar to Null
dereference, but Infer hasn't found a whole trace where the error can happen,
but only found that a null dereference can happen if an instance variable of a
parameter is `nil`. For example:

```objectivec
  -(int) foo {
      B b* = [self->_a foo]; // sending a message with receiver nil returns nil
      return b->x; // dereferencing b, potential NPE if you pass nil as the argument a.
  }
```

Possible solutions are adding a check for `nil`, or making sure that the method
is not called with `nil`.

## Lock Consistency Violation

This is a C++ and Objective C error reported whenever:

- A class contains a member `lock` used for synchronization (most often a
  `std::mutex`).
- It has a public method which writes to some member `x` while holding `lock`.
- It has a public method which reads `x` without holding `lock`.

The above may happen through a chain of calls. Above, `x` may also be a
container (an array, a vector, etc).

### Fixing Lock Consistency Violation reports

- Avoid the offending access (most often the read). Of course, this may not be
  possible.
- Use synchronization to protect the read, by using the same lock protecting the
  corresponding write.
- Make the method doing the read access private. This should silence the
  warning, since Infer looks for a pair of non-private methods. Objective-C:
  Infer considers a method as private if it's not exported in the header-file
  interface.

## Mixed self weakSelf

This happens when an Objective-C block captures both `self` and `weakSelf`, a
weak pointer to `self`. Possibly the developer meant to capture only `weakSelf`
to avoid a retain cycle, but made a typo and used `self` as well in the block,
instead of `strongSelf`. In this case, this could cause a retain cycle.

## Multiple weakSelf

An Objective-C block uses `weakSelf` more than once. This could lead to
unexpected behaviour. Even if `weakSelf` is not nil in the first use, it could
be nil in the following uses since the object that `weakSelf` points to could be
freed anytime. One should assign it to a strong pointer first, and then use it
in the block.

## Memory leak

### Memory leak in C

This error type is only reported in C and Objective-C code. In Java we do not
report memory leaks because it is a garbage collected language.

In C, Infer reports memory leaks when objects are created with `malloc` and not
freed. For example:

```c
-(void) memory_leak_bug {
    struct Person *p = malloc(sizeof(struct Person));
}
```

### Memory leak in Objective-C

Additionally, in Objective-C, Infer reports memory leaks that happen when
objects from Core Foundation or Core Graphics don't get released.

```objectivec
-(void) memory_leak_bug_cf {
    CGPathRef shadowPath = CGPathCreateWithRect(self.inputView.bounds, NULL); //object created and not released.
}
```

## Null Dereference

Infer reports null dereference bugs in C, Objective-C and Java. The issue is
about a pointer that can be `null` and it is dereferenced. This leads to a crash
in all the above languages.

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

In Objective-C, null dereferences are less common than in Java, but they still
happen and their cause can be hidden. In general, passing a message to nil does
not cause a crash and returns `nil`, but dereferencing a pointer directly does
cause a crash as well as calling a `nil` block.C

```objectivec
-(void) foo:(void (^)())callback {
    callback();
}

-(void) bar {
    [self foo:nil]; //crash
}
```

Moreover, there are functions from the libraries that do not allow `nil` to be
passed as argument. Here are some examples:

```objectivec
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

<b> If you are unsure whether or not foo() will return null </b>, you should
ideally i. Change the code to ensure that foo() can not return null ii. Add a
check for whether p is null, and do something other than dereferencing p when it
is null.

Sometimes, in case ii it is not obvious what you should do when p is null. One
possibility (a last option) is to throw an exception, failing early. This can be
done using checkNotNull as in the following code:

```java
  // code idiom for failing early

  import static com.google.common.base.Preconditions.checkNotNull;

  //... intervening code

  p = checkNotNull(foo()); // foo() might return null
  stuff();
  p.goo();   // dereferencing p, potential NPE
```

The call checkNotNull(foo()) will never return null; in case foo() returns null
it fails early by throwing an NPE.

<b> If you are absolutely sure that foo() will not be null </b>, then if you
land your diff this case will no longer be reported after your diff makes it to
master. In the future we might include analysis directives (hey, analyzer, p is
not null!) like in Hack that tell the analyzer the information that you know,
but that is for later.

## Parameter not null checked

This error type is reported only in Objective-C. It is similar to Null
dereference, but Infer hasn't found a whole trace where the error can happen,
but only found that a null dereference can happen if you call a method with nil
as an argument. Therefore it is only a warning. For example:

```objectivec
  -(int) foo:(A* a) {
      B b* = [a foo]; // sending a message with receiver nil returns nil
      return b->x; // dereferencing b, potential NPE if you pass nil as the argument a.
  }
```

or when the parameter is a block:

```objectivec
   -(void) foo:(void (^)(BOOL))block {
      block(YES); // calling a nil block will cause a crash.
   }
```

Possible solutions are adding a check for `nil`, or making sure that the method
is not called with `nil`. When an argument will never be `nil`, you can add the
annotation `nonnull` to the argument's type, to tell Infer (and the type
system), that the argument won't be `nil`. This will silence the warning.

## Premature nil termination argument

This error type is reported in C and Objective-C. In many variadic methods,
`nil` is used to signify the end of the list of input objects. This is similar
to nil-termination of C strings. If one of the arguments that is not the last
argument to the method is `nil` as well, Infer reports an error because that may
lead to unexpected behavior.

An example of such variadic methods is
[arrayWithObjects](https://developer.apple.com/library/prerelease/ios/documentation/Cocoa/Reference/Foundation/Classes/NSArray_Class/index.html#//apple_ref/occ/clm/NSArray/arrayWithObjects)

```objectivec
  NSArray *foo = [NSArray arrayWithObjects: @"aaa", str, @"bbb", nil];
```

In this example, if `str` is `nil` then an array `@[@"aaa"]` of size 1 will be
created, and not an array `@[@"aaa", str, @"bbb"]` of size 3 as expected.

## Resource leak

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
constructor fails it can leave an an unreachable resource that no one can close.

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

## Retain cycle

A retain cycle is a situation when object A retains object B, and object B
retains object A at the same time. Here is an example:

```objectivec
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

You can fix a retain cycle in ARC by using \_\_weak variables or weak properties
for your "back links", i.e. links to direct or indirect parents in an object
hierarchy:

```objectivec
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

## Static initialization order fiasco

This error is reported in C++. It fires when the initialization of a static
variable `A`, accesses a static variable `B` from another translation unit
(usually another `.cpp` file). There are no guarantees whether `B` has been
already initialized or not at that point.

For more technical definition and techniques to avoid/remediate, see the
[FAQ](https://isocpp.org/wiki/faq/ctors#static-init-order).

## Thread-safety violation

This warning indicates a potential data race in Java. The analyser is called
RacerD and this section gives brief but a mostly complete description of its
features. See the [RacerD page](/docs/racerd) for more in-depth information and
examples.

NB this warning **is not related to @GuardedBy** and not issued by the same
analysis.

### Thread-safety: What is a data race

Here a data race is a pair of accesses to the same member field such that:

- at least one is a write, and,
- at least one occurs without any lock synchronization, and,
- the two accesses occur on threads (if known) which can run in parallel.

### Thread-safety: Potential fixes

- Synchronizing the accesses (using the `synchronized` keyword, thread-exclusion
  such as atomic objects, `volatile` etc).
- Making an offending method private -- this will exclude it from being checked
  at the top level, though it will be checked if called by a public method which
  may itself, e.g., hold a lock when calling it.
- Putting the two accesses on the same thread, e.g., by using `@MainThread` or
  `@ThreadConfined`.

### Thread-safety: Conditions checked before reporting

The class and method are not marked `@ThreadSafe(enableChecks = false)`, and,

- The method is declared `synchronized`, or employs (non-transitively) locking,
  or,
- The class is not marked `@NotThreadSafe`, and,
  - The class/method is marked `@ThreadSafe,` or one of the configured synonyms
    in `.inferconfig`, or,
  - A parent class, or an override method are marked with the above annotations.

NB currently RacerD **does not take into account `@GuardedBy`**.

### Thread-safety: Thread annotations recognized by RacerD

These class and method annotations imply the method is on the main thread:
`@MainThread`, `@UiThread`

These method annotations imply the method is on the main thread: `@OnBind`,
`@OnEvent`, `@OnMount`, `@OnUnbind`, `@OnUnmount`

Both classes of annotations work through the inheritance tree (i.e. if a parent
class or method is marked with one of these annotations, so is the child class /
method override).

In addition to these, RacerD recognizes many lifecycle methods as necessarily
running on the main thread, eg `Fragment.onCreate` etc.

Finally, the thread status of being on the main thread propagates backwards
through the call graph (ie if `foo` calls `bar` and `bar` is marked `@UiThtread`
then `foo` is automatically considered on the main thread too). Calling
`assertMainThread`, `assertOnUiThread`, `checkOnMainThread` has the same effect.

NB RacerD currently **does not recognize `@WorkerThread`, `@BinderThread` or
`@AnyThread`**.

### Thread-safety: Other annotations and what they do

These annotations can be found at `com.facebook.infer.annotation.*`.

- `@Functional` This is a method annotation indicating the method always returns
  the same value. When a method `foo` is annotated `@Functional`, RacerD will
  ignore any writes of the return value of `foo`. For example, in
  `this.x = foo()`, the write to `this.x` is ignored. The reasoning is that if
  the method returns the same value whenever it's called, any data race on
  `this.x` is benign, if that is the only write.

- `@ThreadConfined` This is a class/method/field annotation which takes a single
  parameter which can be `UI`, `ANY` or a user chosen string. It indicates to
  RacerD a thread identifier for the class/method/field. Thus,
  `@ThreadConfined(UI)` is equivalent to `@UiThread`, and `@ThreadConfined(ANY)`
  is equivalent to not having the annotation at all, for classes and methods.
  When this annotation is applied to a field it instructs Infer to assume
  (without checking) that all accesses to that field are made on the same thread
  (and can, therefore, not race by definition). The intention is that RacerD
  uses that to detect exclusion between accesses occurring on the same thread.
  However, only the UI thread is supported at this time, and any user provided
  value is considered equal to `UI`.

- `@VisibleForTesting` A method annotation making Infer consider the method as
  effectively `private`. This means it will not be checked for races against
  other non-private methods of the class, but only if called by one.

- `@ReturnsOwnership` A method annotation indicating that the method returns a
  freshly owned object. Accesses to the returned value will not be considered
  for data races, as the object is in-effect unique and not accessible yet from
  other threads. The main utility of this annotation is in interfaces, where
  Infer cannot look up the implementation and decide for itself.

## UI Thread Starvation

This error is reported in Java, and specifically on Android. These reports are
triggered when a method that runs on the UI thread may block, thus potentially
leading to an Application Not Responding error.

Infer considers a method as running on the UI thread whenever:

- The method, one of its overrides, its class, or an ancestral class, is
  annotated with `@UiThread`.
- The method, or one of its overrides is annotated with `@OnEvent`, `@OnClick`,
  etc.
- The method or its callees call a `Litho.ThreadUtils` method such as
  `assertMainThread`.

The issue is reported when a method deemed to run on the UI thread

- Makes a method call which may block.
- Takes a lock, and another thread takes the same lock, and before releasing it,
  makes a call that may block.

Calls that may block are considered:

- Certain I/O calls.
- Two way `Binder.transact` calls.
- Certain OS calls.
- `Future` or `AsyncTask` calls to `get` without timeouts, or with too large
  timeouts.

To suppress starvation reports in a method `m()` use the
`@SuppressLint("STARVATION")` annotation, as follows:

```java
  import android.annotation.SuppressLint;

  @SuppressLint("STARVATION")
  public void m() {
  ...
  }
```

To signal to Infer that a method does not perform any blocking calls, despite
appearences, you can use the `@NonBlocking` annotation:

```java
  import com.facebook.infer.annotation.NonBlocking;

  @NonBlocking
  public void m() {
  ...
  }
```

This instructs Infer to filter out any potentially blocking calls in `m()`
(also, transitively), and thus any other method can expect no starvation reports
due to a call to `m()`. You will need to set up your class path appropriately to
include the JAR files in `infer/annotations` for this annotation to work.

## Strict mode violation

Android has a feature called
[strict mode](https://developer.android.com/reference/android/os/StrictMode),
which if enabled, will flag the occasions where the main thread makes a call
that results in disk I/O, waiting on a network socket, etc. The analysis
catching starvation errors and deadlocks (the `--starvation` analysis) has the
ability to statically detect such violations.

To suppress this warning, it's enough to annotate the offending method with
`@SuppressLint("STRICT_MODE_VIOLATION")`.

## StrongSelf Not Checked

When a block captures `weakSelf` in the following pattern:

```
__weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    int y = strongSelf->x;
```

the variable `strongSelf` should be checked for `null` before being used,
otherwise this could cause a crash because the weak pointer `weakSelf` could be
`null`.

## Unsafe GuardedBy Access

Infer reports issues when a field or method is accessed when a lock is not held,
when the field or method has been annotated with `@GuardedBy(lock)`. In many
cases the lock is `this`. Here is a basic example:

```

import javax.annotation.concurrent.GuardedBy;

class GB{

@GuardedBy("this")
int y;

void foo(){  y = 22; }

void goo(){  synchronized (this) {y = 82;} }

}
```

Infer duly warns on the access to `y` in `foo()`, but not in `goo()`.

```
GB.java:9: error: UNSAFE_GUARDED_BY_ACCESS
  The field `GB.y` is annotated with `@GuardedBy("GB.this")`, but the lock `GB.this` is not held during the access to the field at line 9. Since the current method is non-private, it can be called from outside the current class without synchronization. Consider wrapping the access in a `synchronized(GB.this)` block or making the method private.
  7.   int y;
  8.
  9. > void foo(){  y = 22; }
  10.
  11.   void goo(){  synchronized (this) {y = 82;} }
```

<a name="anonymous_inner"></a> Infer can distinguish between different locks. A
particularly tricky example comes up sometimes where different occurrences of
the keyword `this` in the same file mean different things ("this this is not
that this").

```

class Outer{

@GuardedBy("this")
Object y;

Object foo(){
  return new Object () {
    void m0() {
           synchronized (this)
             { y = null; }
    }
    void m1() {
           synchronized (Outer.this)
             { y = null; }
    }
  };
}

}
```

In this use of "anonymous inner classes" the occurrence of `this` in method
`m0()` refers to the closure created when the new object is created, not to the
`this` that guards `y`. It is a bug, and the fix is to refer to the proper
`this` as in method `m1()`. Infer correctly warns on the access in `m0()` but
not `m1()`.

```
Outer.java:13: error: UNSAFE_GUARDED_BY_ACCESS
  The field `Outer.y` is annotated with `@GuardedBy("Outer.this")`, but the lock `Outer.this` is not held during the access to the field at line 13. Since the current method is non-private, it can be called from outside the current class without synchronization. Consider wrapping the access in a `synchronized(Outer.this)` block or making the method private.
  11.       void m0() {
  12.              synchronized (this)
  13. >              { y = null; }
  14.       }

```

## weakSelf In NOESCAPE Block

In many methods that take a block as an argument, the block position is
annotated with NS_NOESCAPE to mark that the block passed to this method won't be
leaving the current scope. In those cases, there is no need to use `weakSelf` to
avoid the block to capture `self`. This issue type flags this case.
