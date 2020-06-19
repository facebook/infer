This error indicates that you have invoked an interface method not annotated
with `@ThreadSafe` from a thread-safe context (e.g., code that uses locks or is
marked `@ThreadSafe`). The fix is to add the `@ThreadSafe` annotation to the
interface or to the interface method. For background on why these annotations
are needed, see the detailed explanation
[here](/docs/next/checker-racerd#interface-not-thread-safe).
