Pulse detected a Swift code path that reads from an Optional value known
to be `nil`. Examples:

- Force-unwrap of an Optional whose `nil` case is reachable
  (`let x: Int? = nil; let _ = x!`).
- Member access on an Implicitly Unwrapped Optional (`T!`) when the
  underlying value is `nil`.
- A call to `Optional.unsafelyUnwrapped` whose receiver is `nil`.

A `SWIFT_NPE` report means the dereference itself is unsafe -- not that
the surrounding annotation is missing. Suggested fixes:

- Bind the Optional with `if let` or `guard let` before use.
- Provide a default with `??`.
- Use the `?.` chain to short-circuit on `nil`.
- If the value cannot be `nil` by contract, change the type from `T?` to
  `T` (or annotate the Objective-C source as `_Nonnull`) so the
  guarantee is expressed in the signature.
