import Foundation

// --- 1. CLASS METHOD (+) BRIDGE ---
func testObjCClassMethod_good() {
    let revision = LegacyHardware.getRevision()
    assert(revision == 101)
}

func testObjCClassMethod_bad() {
    let revision = LegacyHardware.getRevision()
    assert(revision != 101)
}

// --- 2. INSTANCE METHOD (-) BRIDGE ---
func testObjCInstanceMethod_good() {
    let device = LegacyHardware()
    let status = device.getBatteryStatus()
    assert(status == 75)
}

func testObjCInstanceMethod_bad() {
    let device = LegacyHardware()
    let status = device.getBatteryStatus()
    assert(status != 75)
}

// --- 3. CONDITIONAL DYNAMIC DISPATCH ---

func testConditionalErasure(condition: Bool) -> Int32 {
    let device: AnyObject

    if condition {
        device = LegacyHardware()
    } else {
        device = SomeOtherHardware()
    }

    // The Frontend sees a call on 'AnyObject'.
    // It does not know which class to use for the QualifiedProcName.
    let status = device.getBatteryStatus()
    return status
}

func testConditionalDispatch_True_Path_Good_FP() {
    let status = testConditionalErasure(condition: true)
    assert(status == 75)
}

func testConditionalDispatch_True_Path_Bad() {
    let status = testConditionalErasure(condition: true)
    assert(status != 75)
}

func testConditionalDispatch_False_Path_Good_FP() {
    let status = testConditionalErasure(condition: false)
     assert(status == 50)
}

func testConditionalDispatch_False_Path_Bad() {
    let status = testConditionalErasure(condition: false)
     assert(status != 50)
}

// ------ Implicit nullability

func checkHardware_bad(device: LegacyHardware) {
    let version = device.getFirmwareVersion()! // Force unwrap
    print("Version length: \(version.count)")
}

func testNullability_good(device: LegacyHardware) {
    // 1. Static call (Resolved in Frontend)
    let model = device.getModelName()
    print(model.count) // Safe
}

// --- 5. DYNAMIC DISPATCH NULLABILITY ---
// When the frontend can't statically resolve the ObjC method, the Pulse
// objc_msgSend model resolves it and should still flag missing nullability.

func testDynamicNullability_bad(condition: Bool) {
    let device: AnyObject

    if condition {
        device = LegacyHardware()
    } else {
        device = SomeOtherHardware()
    }

    // getFirmwareVersion has no _Nullable/_Nonnull annotation on either type.
    // Frontend sees AnyObject, can't resolve. Pulse's objc_msgSend model
    // resolves dynamically and should report MISSING_NULLABILITY_ANNOTATION.
    let _ = device.getFirmwareVersion()
}

func testDynamicNullability_good(condition: Bool) {
    let device: AnyObject

    if condition {
        device = LegacyHardware()
    } else {
        device = SomeOtherHardware()
    }

    // getModelName is annotated _Nonnull on both — no report expected.
    let _ = device.getModelName()
}
