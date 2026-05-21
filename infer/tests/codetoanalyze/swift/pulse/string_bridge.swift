// Pulse coverage for the `String` <-> `NSString` Foundation bridges.
//
// `s as NSString` lowers to a call of `String._bridgeToObjectiveC()`;
// `ns as String` to `String._unconditionallyBridgeFromObjectiveC()`.
// Without a Pulse model for these helpers the result register is an
// opaque value with no dynamic type, defeating downstream reasoning
// about the bridged value.
//
// To make the dynamic-type stamp observable we erase the bridged value
// to `AnyObject` (so swiftc emits `swift_getObjectType` for `type(of:)`
// instead of lifting it to a direct metadata field load) and compare
// against an unrelated class metaobject via `__swift_metadata_equals`.
// In this baseline state — without the bridge model — Pulse cannot
// reason about the comparison and the `_good` case fires
// `PULSE_ASSERTION_ERROR` as a false positive.  A later diff in the
// stack adds the bridge model (and depends on the `swift_getObjectType`
// model added in the diff above) to flip the FP and drop the suffix.
//
// The symmetric *equality* assertion (`type(of: bridged) ==
// NSString.self`) is intentionally not exercised here:
// `__swift_metadata_equals` does a structural `Typ.equal` and the
// `NSString` produced by the (future) bridge model differs from the
// `NSString.self` literal in the `SourceFile` of its `SwiftClassName`,
// so the comparison would always evaluate to `false`.  Aligning those
// type representations is a separate follow-up.

import Foundation

class UnrelatedClass {}

func test_bridge_to_nsstring_different_type_bad() {
    let s = "hello"
    let ns: AnyObject = s as NSString
    // type(of: ns) == UnrelatedClass.self -> False (Assertion fails / Bad)
    assert(type(of: ns) == UnrelatedClass.self)
}

func test_bridge_to_nsstring_different_type_good_FP() {
    let s = "hello"
    let ns: AnyObject = s as NSString
    // type(of: ns) != UnrelatedClass.self -> True (Good)
    assert(type(of: ns) != UnrelatedClass.self)
}
