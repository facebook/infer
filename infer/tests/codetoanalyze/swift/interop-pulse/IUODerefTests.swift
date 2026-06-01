// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

// Test corpus for the planned SWIFT_NPE Pulse checker. Each pattern below
// pairs a Swift call into an unannotated / annotated Objective-C boundary
// (or a pure-Swift Optional) with the safety classification the checker
// must reproduce. `_bad` should fire SWIFT_NPE; `_good` must not.

import Foundation

// MARK: - Unannotated ObjC return (Swift sees `String!`)

// Direct force-unwrap of an IUO whose underlying nil-ness is unknown.
func forceUnwrapUnannotated_bad(api: LegacyAPI) {
  let s = api.getUnannotatedString()!
  _ = s.count
}

// Result is discarded -- the bridge runs but its result is never used.
func discardUnannotated_good(api: LegacyAPI) {
  let _ = api.getUnannotatedString()
}

// Optional binding: nil branch is handled by the empty else.
func ifLetUnannotated_good(api: LegacyAPI) {
  if let s = api.getUnannotatedString() {
    _ = s.count
  }
}

// guard-let with early return on nil.
func guardLetUnannotated_good(api: LegacyAPI) {
  guard let s = api.getUnannotatedString() else { return }
  _ = s.count
}

// `??` substitutes a default on nil.
func nilCoalesceUnannotated_good(api: LegacyAPI) {
  let s = api.getUnannotatedString() ?? ""
  _ = s.count
}

// Passthrough into an `-> String?` return signature: nil flows out safely.
func passthroughToOptional_good(api: LegacyAPI) -> String? {
  return api.getUnannotatedString()
}

// Arg-position bridge from `T!` to a `T?` parameter: safe (no deref).
func consumeOptional(_ s: String?) {
  _ = s
}
func passToOptionalParam_good(api: LegacyAPI) {
  consumeOptional(api.getUnannotatedString())
}

// Arg-position bridge from `T!` to a `T` parameter: force-unwrap at call.
func consumeNonOptional(_ s: String) {
  _ = s
}
func passToNonOptionalParam_bad(api: LegacyAPI) {
  consumeNonOptional(api.getUnannotatedString())
}

// Non-binding null check: the result is observed, not dereferenced.
func nonBindingNullCheck_good(api: LegacyAPI) -> Bool {
  return api.getUnannotatedString() != nil
}

// `?.` chain: short-circuits on nil.
func optionalChain_good(api: LegacyAPI) {
  let _ = api.getUnannotatedString()?.count
}

// `!.` chain: force-unwrap before access.
func optionalChainForceUnwrap_bad(api: LegacyAPI) {
  let _ = api.getUnannotatedString()!.count
}

// Same force-unwrap, but the receiver is `AnyObject` so dispatch goes
// through `objc_msgSend`.
func dynamicDispatchForceUnwrap_bad(any: AnyObject) {
  let _ = (any.getUnannotatedString() as AnyObject?)!
}

// MARK: - Annotated ObjC return

// `_Nonnull` return -- Swift sees `String`, no nil possible.
func nonnullDirectDeref_good(api: LegacyAPI) {
  _ = api.getNonnullString().count
}

// `_Nullable` return with `if let`.
func nullableIfLet_good(api: LegacyAPI) {
  if let s = api.getNullableString() {
    _ = s.count
  }
}

// `_Nullable` return with force-unwrap: explicit Optional, programmer
// asserts non-nil. Should fire.
func nullableForceUnwrap_bad(api: LegacyAPI) {
  let _ = api.getNullableString()!.count
}

// Bare return inside `NS_ASSUME_NONNULL_BEGIN` -- implicit `_Nonnull`.
func assumedNonnull_good(api: AssumedNonnullAPI) {
  _ = api.getAssumedNonnullString().count
}

// MARK: - Factory methods (init / new / convention-named)

func factoryInitIfLet_good() {
  if let api = FactoryAPI() {
    _ = api
  }
}

func factoryNewIfLet_good() {
  if let api = FactoryAPI.new() {
    _ = api
  }
}

func factoryConventionIfLet_good() {
  if let api = FactoryAPI.factoryInstance() {
    _ = api
  }
}

// MARK: - NSError out-param convention

func errorOutParam_good(api: ErrorOutParamAPI) {
  do {
    try api.doThing()
  } catch {
    _ = error
  }
}

// MARK: - Pure-Swift Optional (no ObjC boundary)

func pureOptionalIfLet_good() {
  let x: Int? = nil
  if let n = x {
    _ = n
  }
}

func pureOptionalForceUnwrap_bad() {
  let x: Int? = nil
  _ = x!
}

func pureOptionalCoalesce_good() {
  let x: Int? = nil
  let _ = x ?? 0
}

func pureOptionalChain_good() {
  let x: String? = nil
  let _ = x?.count
}

// MARK: - Interprocedural Optional flows (exercise the symbolic-discriminator
// path-split on [Sg]-class storage)

// Helper that unconditionally unwraps its parameter.  Pulse reports
// `SWIFT_NPE` on the helper itself: the symbolic-discriminator path-split now
// fires (`__swift_optional_init_sg` runs at the symbolic-tag site), so the
// `.none` branch of the path-split is reachable in the helper's local
// analysis -- effectively documenting the helper's "caller must pass .some"
// contract.
@inline(never)
func unwrapInner_bad(_ x: Int?) -> Int { x.unsafelyUnwrapped }

// Caller passes literal nil.  Stays silent -- Pulse doesn't re-fire at the
// caller once the helper has reported (caller-site `SWIFT_NPE` would be nicer
// but is a known follow-up).
func interprocUnwrapNil_bad_FN() {
  _ = unwrapInner_bad(nil)
}

func interprocUnwrapSome_good() {
  _ = unwrapInner_bad(5)
}
