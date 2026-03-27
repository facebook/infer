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
