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

// Safe-handling idiom: caller short-circuits on nil. Should not
// fire MISSING_NULLABILITY_ANNOTATION; pinned `_FP` until handled.
func guardLetReturnNil_safeUse_FP(api: LegacyAPI) -> Int? {
  guard let s = api.getUnannotatedString() else { return nil }
  return s.count
}

// Safe-handling idiom: caller substitutes a default for nil. Should
// not fire MISSING_NULLABILITY_ANNOTATION; pinned `_FP` until handled.
func nilCoalesceDefault_safeUse_FP(api: LegacyAPI) -> Int {
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