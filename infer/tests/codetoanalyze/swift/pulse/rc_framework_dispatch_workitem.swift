// Coverage for retain cycles through GCD's `DispatchWorkItem(block:)`.
//
// Cycle: `self -> _workItem -> item -> block -> captured self -> self`.
//
// The BAD case closes a real retain cycle but Pulse currently has no
// model for `DispatchWorkItem.init(qos:flags:block:)`, so the cycle isn't
// visible at the `self.workItem = item` store. Hence the `_FN` suffix on
// `test_dispatch_workitem_self_capture_bad_FN`; the next diff in the
// stack adds the model and drops the suffix.

import Dispatch

// BAD: strong `self` capture in the work item's block; the work item is
// stashed on `self.workItem`, closing the cycle.
final class DispatchWorkItemCycleHolder: @unchecked Sendable {
    var workItem: DispatchWorkItem?
    var label: String = ""

    func startBad() {
        let item = DispatchWorkItem {
            self.label = "fired"
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0, execute: item)
        workItem = item
    }
}

// GOOD: weak self capture — no cycle.
final class DispatchWorkItemWeakHolderGood: @unchecked Sendable {
    var workItem: DispatchWorkItem?
    var label: String = ""

    func startGood() {
        let item = DispatchWorkItem { [weak self] in
            self?.label = "fired"
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0, execute: item)
        workItem = item
    }
}

func test_dispatch_workitem_self_capture_bad_FN() {
    let h = DispatchWorkItemCycleHolder()
    h.startBad()
}

func test_dispatch_workitem_weak_self_capture_good() {
    let g = DispatchWorkItemWeakHolderGood()
    g.startGood()
}
