import Foundation

@objc protocol Tracker {
    @objc func reportStatus() -> Int
}

func testDynamicPropagation(obj: AnyObject) -> Int {
    let methodName = "reportStatus"
    let sel = NSSelectorFromString(methodName)
    let result = obj.perform(sel).takeUnretainedValue()
    return result as! Int
}
