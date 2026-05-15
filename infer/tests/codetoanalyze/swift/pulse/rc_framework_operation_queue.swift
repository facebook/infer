// Coverage for retain cycles through `OperationQueue.addOperation { ... }`.
//
// Cycle: `self -> _queue -> queue -> _operations -> block
//          -> captured self -> self`.
//
// The BAD case closes a real retain cycle (the queue is a property of
// `self`; the queue retains the block while the operation is queued; the
// block captures `self`). Pulse currently has no model for
// `-[NSOperationQueue addOperationWithBlock:]`, so the cycle isn't visible
// at the addOperation call. Hence the `_FN` suffix on
// `test_operation_queue_self_capture_bad_FN`; the next diff in the stack
// adds the model and drops the suffix.

import Foundation

// BAD: strong `self` capture in a block enqueued on a queue owned by `self`.
final class OperationQueueCycleHolder: @unchecked Sendable {
    let queue = OperationQueue()
    var label: String = ""

    func startBad() {
        queue.addOperation {
            self.label = "fired"
        }
    }
}

// GOOD: weak self capture — no cycle.
final class OperationQueueWeakHolderGood: @unchecked Sendable {
    let queue = OperationQueue()
    var label: String = ""

    func startGood() {
        queue.addOperation { [weak self] in
            self?.label = "fired"
        }
    }
}

func test_operation_queue_self_capture_bad_FN() {
    let h = OperationQueueCycleHolder()
    h.startBad()
}

func test_operation_queue_weak_self_capture_good() {
    let g = OperationQueueWeakHolderGood()
    g.startGood()
}
