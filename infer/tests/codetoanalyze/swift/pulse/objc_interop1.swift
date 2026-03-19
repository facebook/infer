
import Foundation

@objc class DeviceManager: NSObject, DeviceProtocol {
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

@objc protocol DeviceProtocol: NSObjectProtocol {
    // Instance method (Objective-C: -)
    @objc func startScanning() -> Int

    // Class method (Objective-C: +)
    @objc static func resetAllDevices() -> Int
}

func testProtocolInstance(device: any DeviceProtocol) -> Int {
    // We use NSSelectorFromString to hide the method name from the
    // Swift type-checker so it can't "help" us.
    let sel = NSSelectorFromString("startScanning")
    let target = device as AnyObject
    let result = target.perform(sel).takeUnretainedValue()
    return result as! Int
}

func testProtocolClass(deviceType: any DeviceProtocol.Type) -> Int {
    let sel = NSSelectorFromString("resetAllDevices")
    let target = deviceType as AnyObject
    let result = target.perform(sel).takeUnretainedValue()
    return result as! Int
}
// A wrapper to see if Pulse can propagate values through the protocol
func testEndToEndInstanceProtocol_bad() {
    let manager = DeviceManager()
    // Cast to protocol to force the objc_msgSend path
    let proto: any DeviceProtocol = manager
    assert(testProtocolInstance(device: proto) != 5)
}

func testEndToEndInstanceProtocol_good_fp() {
    let manager = DeviceManager()
    // Cast to protocol to force the objc_msgSend path
    let proto: any DeviceProtocol = manager
    assert(testProtocolInstance(device: proto) == 5)
}

func testEndToEndClassProtocol_bad() {
    let manager = DeviceManager()
    let protoType: any DeviceProtocol.Type = type(of: manager)
    assert(testProtocolClass(deviceType: protoType) != 10)
}

func testEndToEndClassProtocol_good_fp() {
    let manager = DeviceManager()
    let protoType: any DeviceProtocol.Type = type(of: manager)
    assert(testProtocolClass(deviceType: protoType) == 10)
}
