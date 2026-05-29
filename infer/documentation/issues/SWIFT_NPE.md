Pulse detected a Swift code path that reads from an Optional value known
to be `nil`. The dereference itself is unsafe -- not the surrounding
type annotation.

The check is Swift-specific and considers an unwrap unsafe only when the
`.none` case is reachable along some path. Unwraps on values that are
known `.some` (literal payload, prior `if let` / `guard let` binding,
`??` default) are silent. Unknown receivers (e.g. the result of an
unmodelled extern call) are also silent.

## Force-unwrap of an Optional whose `nil` case is reachable

```swift
func unwrapsKnownNil_bad() {
  let x: Int? = nil
  _ = x!                 // SWIFT_NPE: x is known .none here
}
```

Fix by binding the value before use:

```swift
func unwrapsKnownNil_good() {
  let x: Int? = nil
  if let v = x {
    _ = v                 // OK: only reached on the .some branch
  }
}
```

Or provide a default:

```swift
func defaultsToZero_good() {
  let x: Int? = nil
  let _ = x ?? 0          // OK: no force-unwrap
}
```

## Implicitly Unwrapped Optional (`T!`) whose underlying value is `nil`

`T!` types are written as non-optional but compile to `Optional<T>` and
trap on access if the value is `nil`.

```swift
func iuoFromBridge_bad(api: LegacyAPI) {
  let s: String! = api.getUnannotatedString()   // ObjC returns nullable
  _ = s.count                                   // SWIFT_NPE on .none
}
```

Treat the value as a regular Optional and bind it explicitly:

```swift
func iuoBound_good(api: LegacyAPI) {
  guard let s = api.getUnannotatedString() else { return }
  _ = s.count                                   // OK
}
```

If the underlying source can be annotated, mark the ObjC return
`_Nonnull` (or change the Swift type from `T?` / `T!` to `T`) so the
guarantee shows up in the signature.

## `Optional.unsafelyUnwrapped` on a known-`nil` receiver

```swift
func unsafelyUnwrappedNil_bad() {
  let x: Int? = nil
  _ = x.unsafelyUnwrapped       // SWIFT_NPE
}
```

`unsafelyUnwrapped` has the same trap semantics as `!` but skips the
runtime check in optimised builds. Prefer the same fixes as above
(`if let`, `guard let`, `??`).
