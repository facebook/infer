// Force-unwrap (postfix !) lowering shapes, for observing the
// Llair2TextualForceUnwrap rewrite at the Textual/SIL level. At the bottom of
// the stack every trap is `__sil_assert_fail`; each recogniser diff rewrites
// the trap it newly recognises to `__swift_optional_force_unwrap_trap`, which
// the companion Pulse model fires `SWIFT_NPE` on.

// Branch-form diamond: force-unwrap of a symbolic Optional parameter.
public func diamondForceUnwrap(_ x: Int?) -> Int {
  return x!
}

// Constant-folded: statically-nil Optional, discriminator check folded away.
public func constFoldedForceUnwrap() -> Int {
  let x: Int? = nil
  return x!
}

// Symbolic-discriminator path-split: a helper that force-unwraps its Optional
// parameter; the discriminator arrives as a runtime tag.
@inline(never)
public func pathSplitForceUnwrap(_ x: Int?) -> Int {
  return x.unsafelyUnwrapped
}
