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

// `Optional.unsafelyUnwrapped`: SWIFT_NPE on a proven [.none] receiver.
func unsafelyUnwrappedNil_bad() {
  let x: Int? = nil
  _ = x.unsafelyUnwrapped
}

// Statically [.some(_)] -- expected to stay silent under any model.
func unsafelyUnwrappedSome_good() {
  let x: Int? = .some(5)
  _ = x.unsafelyUnwrapped
}

// Class-typed payload variant (`Optional<String>`).
func unsafelyUnwrappedNilString_bad() {
  let x: String? = nil
  _ = x.unsafelyUnwrapped
}

func unsafelyUnwrappedSomeString_good() {
  let x: String? = .some("hi")
  _ = x.unsafelyUnwrapped
}

// Receiver flows from an unmodelled extern: heap can't prove `.none`,
// so any nil-deref checker should stay silent.
func unsafelyUnwrappedUnknown_good(api: LegacyAPI) {
  let x: String? = api.getNullableString()
  _ = x.unsafelyUnwrapped
}

// The payload propagates through unwrap: with the receiver statically [.some(5)],
// the unwrapped value is tracked as 5 and the impossible division-by-zero branch
// is pruned.
func unsafelyUnwrappedPayloadPropagates_good() {
  let x: Int? = .some(5)
  _ = 10 / x.unsafelyUnwrapped
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

// MARK: - Safe optional access on a proven-[.none] value (FP)

// A callee that provably returns nil, read back through a *safe* unwrap. These
// are the shapes that dominated the first real-app SWIFT_NPE run: none of them
// involves an explicit `!`, so none should ever fire SWIFT_NPE. Optional
// chaining / nil-coalescing are already silent; `guard let` / `if let` followed
// by a field access on the binding currently FP (the `.none` Optional reaches
// the safe-unwrap branch). The `_FP` cases are fixed by the follow-up model
// change; this diff pins the current behaviour.
struct BoxAdv { let value: Int? }
func returnsNilBoxAdv() -> BoxAdv? { return nil }
func returnsNilIntAdv() -> Int? { return nil }

// Optional chaining / nil-coalescing on a proven-`.none`: already silent.
func optionalChainProvenNil_good() -> Int {
  let b: BoxAdv? = returnsNilBoxAdv()
  return b?.value ?? 0
}

func coalesceProvenNil_good() -> Int {
  let x: Int? = returnsNilIntAdv()
  return x ?? 0
}

// `guard let` / `if let` then a field access on the binding: currently a
// spurious SWIFT_NPE on the proven-`.none` value.
func guardLetProvenNil_good_FP() -> Int {
  guard let b = returnsNilBoxAdv() else { return -1 }
  return b.value ?? 0
}

func ifLetProvenNil_good_FP() -> Int {
  if let b = returnsNilBoxAdv(), let v = b.value { return v }
  return 0
}
