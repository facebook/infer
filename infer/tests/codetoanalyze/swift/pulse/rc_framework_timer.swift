// Coverage for retain cycles via real Apple framework APIs that retain
// closures capturing `self`. Companion to `rc_closure_self_capture.swift`,
// which uses a synthetic `external_register_handler` stub; this fixture
// exercises the actual ObjC framework procnames as Pulse sees them after
// Swift -> ObjC bridging through `objc_msgSend`.

import Foundation

// BAD: Timer.scheduledTimer retains the block; the block captures `self`
// strongly; the resulting Timer is stored on `self.timer`. Closes:
//   `self -> timer -> block -> captured self -> self`.
final class TimerCycleHolder: @unchecked Sendable {
    var timer: Timer?
    var label: String = ""

    func startBad() {
        timer = Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { _ in
            self.label = "tick"
        }
    }
}

// GOOD: weak `self` capture in the block — no cycle.
final class TimerWeakHolderGood: @unchecked Sendable {
    var timer: Timer?
    var label: String = ""

    func startGood() {
        timer = Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { [weak self] _ in
            self?.label = "tick"
        }
    }
}

func test_timer_self_capture_bad() {
    let h = TimerCycleHolder()
    h.startBad()
}

func test_timer_weak_self_capture_good() {
    let g = TimerWeakHolderGood()
    g.startGood()
}
