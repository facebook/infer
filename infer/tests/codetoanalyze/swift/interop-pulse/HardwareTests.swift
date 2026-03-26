import Foundation

// --- 1. CLASS METHOD (+) BRIDGE ---
func testObjCClassMethod_good_fp() {
    let revision = LegacyHardware.getRevision()
    assert(revision == 202)
}

func testObjCClassMethod_bad() {
    let revision = LegacyHardware.getRevision()
    assert(revision == 999)
}

// --- 2. INSTANCE METHOD (-) BRIDGE ---
func testObjCInstanceMethod_good_fp() {
    let device = LegacyHardware()
    let status = device.getBatteryStatus()
    assert(status == 88)
}

func testObjCInstanceMethod_bad() {
    let device = LegacyHardware()
    let status = device.getBatteryStatus()
    assert(status == 0)
}
