This warning indicates a potential data race in Java. The analyser is called
RacerD and this section gives brief but a mostly complete description of its
features. See the [RacerD page](/docs/next/checker-racerd) for more in-depth information and
examples.

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
