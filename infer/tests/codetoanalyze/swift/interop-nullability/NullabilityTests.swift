import Foundation

func unannotatedReturn_bad(api: LegacyAPI) {
  let s = api.getUnannotatedString()!
  print(s.count)
}

func nonnullReturn_good(api: LegacyAPI) {
  let s = api.getNonnullString()
  print(s.count)
}

func nullableReturn_good(api: LegacyAPI) {
  if let s = api.getNullableString() {
    print(s.count)
  }
}

func macroAnnotatedNullableProp_good(api: LegacyAPI) {
  if let s = api.macroAnnotatedNullableProp {
    print(s.count)
  }
}

func macroAnnotatedUnannotatedProp_bad(api: LegacyAPI) {
  let s = api.macroAnnotatedUnannotatedProp!
  print(s.count)
}

// The .h's category interface is gated by `#ifdef __swift__`, so Swift's
// clang importer sees the method as returning `String?` (Optional) but
// infer's ObjC capture sees only the .m's bare `NSString*`. The `!` here
// force-unwraps an Optional the user knows is nullable; infer should not
// flag it. Today it does (FP), so the test is `_FP_*` until we fix the
// checker.
func swiftRefinedNullableString_good_FP(api: LegacyAPI) {
  let s = api.swiftRefinedNullableString()!
  print(s.count)
}

// Recommended developer workaround when the ObjC header cannot be
// annotated: cast the result to an Optional via `as?`. The LLAIR
// produced for an `as?` cast contains a downstream call to
// `_bridgeToObjectiveC`, which the bridge-analysis prepass uses as the
// signature of an explicit Optional cast. The Llair-to-Textual
// translator then attaches `Nullable` to the call's
// `cf_caller_ret_annots`, suppressing the would-be MISSING_NULLABILITY
// report for this defensive use of an unannotated method.
func unannotatedReturn_castAsOptional_good(api: LegacyAPI) {
  if let s = api.getUnannotatedString() as? NSString {
    print(s.length)
  }
}

// `NS_ASSUME_NONNULL_BEGIN/END` is the dominant ObjC-annotation style.
// Bare `NSString*` inside the block is implicitly `_Nonnull`, so Swift
// imports the result as `String` and infer should not flag a
// missing-nullability annotation.
func assumeNonnullBlock_good(api: AssumedNonnullAPI) {
  let s = api.getAssumedNonnullString()
  print(s.count)
}

// Inside the same block, an explicit `nullable` override is honored. The
// declaration carries an annotation, so MISSING_NULLABILITY must not fire
// (the call site uses `if let`, which is the safe pattern).
func nullableOverrideInsideAssumeNonnullBlock_good(api: AssumedNonnullAPI) {
  if let s = api.getExplicitlyNullableInsideBlock() {
    print(s.count)
  }
}

// Surprisingly, unannotated `- (instancetype)init`,
// `+ (instancetype)new`, and convention-named factory methods are NOT
// auto-treated as nonnull by Swift's clang importer when they're outside
// an `NS_ASSUME_NONNULL_BEGIN` block: they're all imported as
// `FactoryAPI?`. The Swift caller is forced to handle the nil case, so
// the safe `if let` pattern is crash-free regardless of what the checker
// says. These tests pin the current MISSING_NULLABILITY behavior on
// safe-Swift-side use of unannotated factories -- a shipping-readiness
// FP-rate signal we want to track.
func factoryInit_safeUse() {
  if let api = FactoryAPI() {
    print(api)
  }
}

// `+new` and convention-named factories return an unannotated pointer,
// but the Swift caller handles the optional safely via `if let`. The
// frontend's null-check inference pass sees the caller's null check and
// suppresses MISSING_NULLABILITY here.
func factoryNew_safeUse() {
  if let api = FactoryAPI.new() {
    print(api)
  }
}

func factoryClassMethod_safeUse() {
  if let api = FactoryAPI.factoryInstance() {
    print(api)
  }
}

// Cocoa's `NSError**` out-parameter convention: a non-pointer return
// (here `BOOL`) paired with a trailing `NSError**`. The return type is
// not a pointer, so MISSING_NULLABILITY must not fire even though the
// `NSError**` parameter itself carries no annotation.
func errorOutParam_good(api: ErrorOutParamAPI) {
  do {
    try api.doThing()
  } catch {
    print(error)
  }
}

// Class methods (`+`) should be reported the same as instance methods
// (`-`) when the pointer return is unannotated. This guards against the
// checker accidentally exempting one method kind.
func classMethodUnannotated_bad() {
  let s = ClassMethodAPI.classGetUnannotatedString()!
  print(s.count)
}

// Safe-handling idiom: caller short-circuits on nil. The post-bridge
// `guard let` null check makes the subsequent use safe.
func guardLetReturnNil_safeUse_good(api: LegacyAPI) -> Int? {
  guard let s = api.getUnannotatedString() else { return nil }
  return s.count
}

// Same idiom with a non-trivial else: a non-fatal logging call before
// the value-returning Return. Mirrors the dominant fbobjc shape:
// `guard let X = e else { FBReportMustFix(...); return false }`.
@inline(never)
func nonFatalLog(_ msg: String) { print(msg) }

func guardLetWithLogReturnFalse_safeUse_good(api: LegacyAPI) -> Bool {
  guard let s = api.getUnannotatedString() else {
    nonFatalLog("getUnannotatedString returned nil")
    return false
  }
  return s.count > 0
}

// Safe-handling idiom: caller substitutes a default for nil. The
// post-bridge `??` null check covers any subsequent use.
func nilCoalesceDefault_safeUse_good(api: LegacyAPI) -> Int {
  let s = api.getUnannotatedString() ?? ""
  return s.count
}

// Optional-returning passthrough: the only use of the unannotated
// result is to return it through an `-> T?` signature. Swift's type
// system already forces every caller to handle nil, so there is no
// crash risk. Should not fire MISSING_NULLABILITY_ANNOTATION.
func optionalGetterPassthrough_safeUse_good(api: LegacyAPI) -> String? {
  return api.getUnannotatedString()
}

// Same passthrough idiom with a one-line let binding between the call
// and the return. Source-equivalent; should not fire either.
func optionalGetterPassthroughViaLet_safeUse_good(api: LegacyAPI) -> String? {
  let s = api.getUnannotatedString()
  return s
}

// Negative case: same Optional-returning shape, but the body
// dereferences the unannotated result mid-body. The deref is a real
// risk and MUST still be reported -- pins the suppression's blast
// radius.
func optionalGetterDerefMidBody_bad(api: LegacyAPI) -> Int? {
  let s = api.getUnannotatedString()
  let n = s!.count
  return n
}

// Argument-position passthrough: the unannotated call result flows
// directly into another function's Optional<T> parameter. Swift bridges
// `T! -> T?` at the call boundary with no deref, so the receiving
// function safely sees an Optional and MISSING_NULLABILITY should not
// fire on the original call.
func consumeOptional(_ s: String?) {
  print(s ?? "default")
}

func argPositionPassthroughOptional_safeUse_good(api: LegacyAPI) {
  consumeOptional(api.getUnannotatedString())
}

// Negative case: the receiving param is non-Optional, so Swift force-unwraps
// `T! -> T` at the call boundary. Real crash risk -- pins the Pattern 5
// recogniser's blast radius.
func consumeNonOptional(_ s: String) {
  print(s)
}

func argPositionForceUnwrappedAtCall_bad(api: LegacyAPI) {
  consumeNonOptional(api.getUnannotatedString())
}

// Multi-branch passthrough: the unannotated call sits in one arm of an
// if/else returning Optional<T>; the other arm returns a different
// Optional. The result still funnels through `-> T?` with no deref, so
// the recogniser must extend its single-branch coverage to here too.
func multiBranchPassthrough_safeUse_good(api: LegacyAPI, useApi: Bool, fallback: String?) -> String? {
  if useApi {
    return api.getUnannotatedString()
  } else {
    return fallback
  }
}

// Same shape with an Optional-chain receiver in the call arm.
func multiBranchPassthroughOptionalChain_safeUse_good(api: LegacyAPI?, useApi: Bool, fallback: String?) -> String? {
  if useApi {
    return api?.getUnannotatedString()
  } else {
    return fallback
  }
}

// Three-branch variant: if / else if / fall-through `return nil` adds a
// third merge predecessor to the join block.
func threeBranchPassthroughOptionalChain_safeUse_good(api: LegacyAPI?, useFlagA: Bool, useFlagB: Bool, fallback: String?) -> String? {
  if useFlagA {
    return api?.getUnannotatedString()
  } else if useFlagB {
    return fallback
  }
  return nil
}

// Computed property `var foo: T? { ... }` -- a class getter rather
// than a top-level func, so the SIL signature has the implicit `self`.
class MultiBranchPassthroughGetter {
  let api: LegacyAPI?
  let fallback: String?
  init(api: LegacyAPI?, fallback: String?) { self.api = api; self.fallback = fallback }

  var multiBranchGetter_safeUse_good: String? {
    if api != nil {
      return api?.getUnannotatedString()
    } else if fallback != nil {
      return fallback
    }
    return nil
  }
}

// Optional-chain on the direct call result: `?.` short-circuits on
// nil, so no deref happens regardless of the unannotated callee's
// nullability. Should not fire MISSING_NULLABILITY_ANNOTATION.
func optionalChainOnDirectCall_safeUse_good(api: LegacyAPI) {
  let _ = api.getUnannotatedString()?.count
}

// Same `?.` idiom with the call result bound to a local first.
// Source-equivalent; should not fire either.
func optionalChainViaLet_safeUse_good(api: LegacyAPI) {
  let s = api.getUnannotatedString()
  let _ = s?.count
}

// Negative case: same `?.`-eligible call, but the caller force-unwraps
// the result instead of chaining. Real crash risk, MUST still be
// reported -- pins the Pattern-2 recogniser's blast radius.
func optionalChainEligibleButForceUnwrapped_bad(api: LegacyAPI) {
  let _ = api.getUnannotatedString()!.count
}