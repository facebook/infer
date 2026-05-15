// Coverage for retain cycles through GCD
// `DispatchSourceProtocol.{setEventHandler, setCancelHandler}`.
//
// Cycle: `self -> _timer -> source -> _{event,cancel}_handler -> closure
//          -> captured self -> self`.
//
// Both BAD cases close a real retain cycle through the Swift Dispatch overlay's
// `set{Event,Cancel}Handler` block-retention; the matchers in PulseModelsSwift
// stash the block as a strong field on the source under `__infer_{event,cancel}_handler`,
// closing the cycle at the `self.timer = source` store.

import Dispatch

// BAD: strong `self` capture in the source's event handler.
final class DispatchTimerCycleHolder: @unchecked Sendable {
    var timer: DispatchSourceTimer?
    var label: String = ""

    func startBad() {
        let t = DispatchSource.makeTimerSource(queue: .main)
        t.schedule(deadline: .now() + 1.0, repeating: 1.0)
        t.setEventHandler {
            self.label = "tick"
        }
        t.resume()
        timer = t
    }
}

// BAD: strong `self` capture in the source's cancel handler.
final class DispatchTimerCancelCycleHolder: @unchecked Sendable {
    var timer: DispatchSourceTimer?
    var label: String = ""

    func startBad() {
        let t = DispatchSource.makeTimerSource(queue: .main)
        t.schedule(deadline: .now() + 1.0, repeating: 1.0)
        t.setCancelHandler {
            self.label = "cancelled"
        }
        t.resume()
        timer = t
    }
}

// GOOD: weak self capture in the event handler — no cycle.
final class DispatchTimerWeakHolderGood: @unchecked Sendable {
    var timer: DispatchSourceTimer?
    var label: String = ""

    func startGood() {
        let t = DispatchSource.makeTimerSource(queue: .main)
        t.schedule(deadline: .now() + 1.0, repeating: 1.0)
        t.setEventHandler { [weak self] in
            self?.label = "tick"
        }
        t.resume()
        timer = t
    }
}

// GOOD: weak self capture in the cancel handler — no cycle.
final class DispatchTimerCancelWeakHolderGood: @unchecked Sendable {
    var timer: DispatchSourceTimer?
    var label: String = ""

    func startGood() {
        let t = DispatchSource.makeTimerSource(queue: .main)
        t.schedule(deadline: .now() + 1.0, repeating: 1.0)
        t.setCancelHandler { [weak self] in
            self?.label = "cancelled"
        }
        t.resume()
        timer = t
    }
}

func test_dispatch_timer_self_capture_bad() {
    let h = DispatchTimerCycleHolder()
    h.startBad()
}

func test_dispatch_timer_cancel_self_capture_bad() {
    let h = DispatchTimerCancelCycleHolder()
    h.startBad()
}

func test_dispatch_timer_weak_self_capture_good() {
    let g = DispatchTimerWeakHolderGood()
    g.startGood()
}

func test_dispatch_timer_cancel_weak_self_capture_good() {
    let g = DispatchTimerCancelWeakHolderGood()
    g.startGood()
}
