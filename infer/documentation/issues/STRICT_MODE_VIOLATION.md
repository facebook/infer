Android has a feature called
[strict mode](https://developer.android.com/reference/android/os/StrictMode),
which if enabled, will flag the occasions where the main thread makes a call
that results in disk I/O, waiting on a network socket, etc. The analysis
catching starvation errors and deadlocks (the `--starvation` analysis) has the
ability to statically detect such violations.

To suppress this warning, it's enough to annotate the offending method with
`@SuppressLint("STRICT_MODE_VIOLATION")`.
