// Coverage for retain cycles through Swift's modern KVO closure form
// `NSObject.observe(_:options:changeHandler:)`.
//
// Cycle: `self -> _observation -> token -> _changeHandler -> closure
//          -> captured self -> self`.
//
// The BAD case closes a real retain cycle through the Foundation
// `_KeyValueCodingAndObserving.observe` extension; the matcher in
// PulseModelsSwift wraps the call in a closure-holder whose
// `_captured_env` strong field points back at `self`, closing the cycle
// at the `self.observation = result` store.

import Foundation

class KVOSubject: NSObject {
    @objc dynamic var value: Int = 0
}

// BAD: strong `self` capture in the change handler; the returned token
// is stashed on `self.observation`, closing the cycle.
final class KVOClosureCycleHolder: @unchecked Sendable {
    var observation: NSKeyValueObservation?
    var subject: KVOSubject = KVOSubject()
    var label: String = ""

    func startBad() {
        observation = subject.observe(\.value, options: [.new]) { _, _ in
            self.label = "changed"
        }
    }
}

// GOOD: weak self capture — no cycle.
final class KVOClosureWeakHolderGood: @unchecked Sendable {
    var observation: NSKeyValueObservation?
    var subject: KVOSubject = KVOSubject()
    var label: String = ""

    func startGood() {
        observation = subject.observe(\.value, options: [.new]) { [weak self] _, _ in
            self?.label = "changed"
        }
    }
}

func test_kvo_closure_self_capture_bad() {
    let h = KVOClosureCycleHolder()
    h.startBad()
}

func test_kvo_closure_weak_self_capture_good() {
    let g = KVOClosureWeakHolderGood()
    g.startGood()
}
