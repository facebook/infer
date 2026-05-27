// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

// Frontend fixture for the Swift Optional construction rewrite:
// the [Llair2Textual] pass recognises `Optional<T>.field_1 <- 0|1`
// stores on a class whose Swift mangled name ends in [Sg] and
// rewrites them to calls to the
// [$builtins.__swift_optional_init_{none,some}] builtins.  See
// [Llair2Textual.try_rewrite_optional_discriminator_store] and
// [SwiftProcname.OptionalInit{None,Some}].

@inline(never)
func sink(_ x: Int?) {
  _ = x
}

func optional_none() {
  let x: Int? = nil
  sink(x)
}

func optional_some() {
  let x: Int? = .some(42)
  sink(x)
}
