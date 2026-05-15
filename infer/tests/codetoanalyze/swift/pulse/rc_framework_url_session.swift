// Coverage for retain cycles through `URLSession.dataTask(with:completionHandler:)`.
//
// Cycle: `self -> _task -> task -> _completionHandler -> closure
//          -> captured self -> self`.
//
// The BAD case closes a real retain cycle through
// `URLSession.dataTask(with:completionHandler:)`; the matcher in
// PulseModelsObjC wraps the call in a closure-holder whose `_captured_env`
// strong field points back at the heap-copied completion block (which itself
// carries the captured-self path), closing the cycle at the
// `self.task = result` store.

import Foundation

// BAD: strong `self` capture in the dataTask's completion handler; the
// returned `URLSessionDataTask` is stashed on `self.task`, closing the
// cycle.
final class URLSessionTaskCycleHolder: @unchecked Sendable {
    var task: URLSessionDataTask?
    var label: String = ""

    func startBad(url: URL) {
        task = URLSession.shared.dataTask(with: url) { _, _, _ in
            self.label = "received"
        }
    }
}

// GOOD: weak self capture — no cycle.
final class URLSessionTaskWeakHolderGood: @unchecked Sendable {
    var task: URLSessionDataTask?
    var label: String = ""

    func startGood(url: URL) {
        task = URLSession.shared.dataTask(with: url) { [weak self] _, _, _ in
            self?.label = "received"
        }
    }
}

func test_url_session_self_capture_bad(url: URL) {
    let h = URLSessionTaskCycleHolder()
    h.startBad(url: url)
}

func test_url_session_weak_self_capture_good(url: URL) {
    let g = URLSessionTaskWeakHolderGood()
    g.startGood(url: url)
}
