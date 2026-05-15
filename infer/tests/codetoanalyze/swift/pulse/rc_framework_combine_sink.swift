// Coverage for retain cycles through Combine `Publisher.sink(receiveValue:)`.
//
// Cycle: `self -> _cancellable -> token -> _captured_env -> self`.
//
// The BAD case closes a real retain cycle through the Combine
// `Publisher.sink(receiveValue:)` extension; the matcher in PulseModelsSwift
// wraps the call in a closure-holder whose `_captured_env` strong field
// points back at `self`, closing the cycle at the
// `self.cancellable = result` store.

import Combine
import Foundation

// BAD: strong `self` capture in `.sink`'s `receiveValue` closure; the returned
// `AnyCancellable` is stashed on `self.cancellable`, closing the cycle.
final class CombineSinkCycleHolder: @unchecked Sendable {
    var cancellable: AnyCancellable?
    var subject = PassthroughSubject<Int, Never>()
    var label: String = ""

    func startBad() {
        cancellable = subject.sink { _ in
            self.label = "received"
        }
    }
}

// GOOD: weak self capture — no cycle.
final class CombineSinkWeakHolderGood: @unchecked Sendable {
    var cancellable: AnyCancellable?
    var subject = PassthroughSubject<Int, Never>()
    var label: String = ""

    func startGood() {
        cancellable = subject.sink { [weak self] _ in
            self?.label = "received"
        }
    }
}

func test_combine_sink_self_capture_bad() {
    let h = CombineSinkCycleHolder()
    h.startBad()
}

func test_combine_sink_weak_self_capture_good() {
    let g = CombineSinkWeakHolderGood()
    g.startGood()
}
