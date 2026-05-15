// Coverage for retain cycles through Swift's modern KVO closure form
// `NSObject.observe(_:options:changeHandler:)`.
//
// Cycle: `self -> _observation -> token -> _changeHandler -> closure
//          -> captured self -> self`.
//
// The BAD case closes a real retain cycle but Pulse currently has no
// model for the Foundation `_KeyValueCodingAndObserving.observe` extension,
// so the cycle isn't visible at the `self.observation = result` store.
// Hence the `_FN` suffix on `test_kvo_closure_self_capture_bad_FN`; the
// next diff in the stack adds the model and drops the suffix.

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

func test_kvo_closure_self_capture_bad_FN() {
    let h = KVOClosureCycleHolder()
    h.startBad()
}

func test_kvo_closure_weak_self_capture_good() {
    let g = KVOClosureWeakHolderGood()
    g.startGood()
}
