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
