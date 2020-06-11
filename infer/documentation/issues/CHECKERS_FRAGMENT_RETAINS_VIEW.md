This error type is Android-specific. It fires when a `Fragment` type fails to
nullify one or more of its declared `View` fields in `onDestroyView`. In
performance-sensitive applications, a `Fragment` should initialize all `View`'s
in `onCreateView` and nullify them in `onDestroyView`. If a `Fragment` is placed
on the back stack and fails to nullify a `View` in `onDestroyView`, it will
retain a useless reference to that `View` that will not be cleaned up until the
`Fragment` is resumed or destroyed.

Action: Nullify the `View` in question in `onDestroyView`.
