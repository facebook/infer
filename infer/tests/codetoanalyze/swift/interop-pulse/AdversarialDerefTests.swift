// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

// Adversarial cases for the planned SWIFT_NPE Pulse checker. These
// probe nil-deref shapes that don't involve an explicit `!` operator,
// patterns that Pulse may silently miss today, and patterns that look
// suspicious but are safe and must not become FPs.

import Foundation

// MARK: - Implicit unwraps (no explicit `!` written by the user)

// `T!` member access. Swift inserts an implicit unwrap; should trap if nil.
func silentReceiverDeref_bad(api: LegacyAPI) {
  let s: String! = api.getUnannotatedString()
  _ = s.count
}

// IUO chained directly into a method call without rebinding.
func iuoIntoArg_bad(api: LegacyAPI) {
  let s: String! = api.getUnannotatedString()
  consumeNonOptionalAdv(s)
}
func consumeNonOptionalAdv(_ s: String) {
  _ = s
}

// MARK: - Pure-Swift collection / cast unwraps

func dictSubscriptForceUnwrap_bad() {
  let dict: [String: String] = [:]
  _ = dict["k"]!
}

func arrayFirstForceUnwrap_bad() {
  let arr: [Int] = []
  _ = arr.first!
}

// `nil as! T` traps -- the cast cannot manufacture a non-nil result.
func asCastNilToClass_bad() {
  let x: AnyObject? = nil
  let _ = x as! NSString
}

// `Optional.unsafelyUnwrapped` is the documented unsafe-fast unwrap.
// Currently silently missed -- Pulse has no model for it.
func unsafelyUnwrappedNil_bad_FN() {
  let x: Int? = nil
  _ = x.unsafelyUnwrapped
}

// MARK: - Likely-FP `_good` cases (must NOT fire)

// `[weak self]` closure with `?.` -- nil-safe by construction.
class WeakSelfHolder {
  var name: String = "x"
  func setup() {
    schedule { [weak self] in
      _ = self?.name
    }
  }
}
func schedule(_ block: @escaping () -> Void) {
  block()
}
func weakSelfClosure_good() {
  WeakSelfHolder().setup()
}

// `lazy var` is initialised on first access; never observably nil.
class LazyHolder {
  lazy var derived: String = computeDerived()
  func computeDerived() -> String { return "x" }
}
func lazyVarFirstUse_good() {
  let h = LazyHolder()
  _ = h.derived
}

// `guard let ... else { fatalError() }` -- the post-guard use is on a
// statically non-nil binding. Currently fires at the `guard` because
// Pulse does not model `fatalError()` as non-returning, so the `s.count`
// site looks reachable on the `.none` path.
func guardLetFatalError_good_FP(api: LegacyAPI) -> Int {
  guard let s = api.getUnannotatedString() else { fatalError("unreachable") }
  return s.count
}

// `switch .some(let x)` binding -- `x` is non-nil in the `.some` arm.
func switchSomeBinding_good(api: LegacyAPI) -> Int {
  switch api.getNullableString() {
  case .some(let s):
    return s.count
  case .none:
    return 0
  }
}
