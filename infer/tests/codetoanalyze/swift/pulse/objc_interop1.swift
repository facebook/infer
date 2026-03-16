
import Foundation

@objc class DeviceManager: NSObject {
    // Instance method
    @objc @inline(never)  func startScanning() -> Int {
        return 5
    }

    // Class method
    @objc @inline(never) static func resetAllDevices() -> Int {
        return 10
    }
}

// By allocating inside, Pulse sees:
// 1. Allocation of DeviceManager
// 2. Call to startScanning on that specific instance
// 3. Constant 5 being propagated
func testSummaryInstance_bad() {
    let manager = DeviceManager()
    assert(manager.startScanning() != 5)
}

func testSummaryInstance_good() {
    let manager = DeviceManager()
    assert(manager.startScanning() == 5)
}

func testInteropClass_bad() {
    assert(DeviceManager.resetAllDevices() != 10)
}

func testInteropClass_good() {
    assert(DeviceManager.resetAllDevices() == 10)
}
