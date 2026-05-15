// Coverage for retain cycles through `URLSession`'s sibling `dataTask` /
// `downloadTask` overloads — same shape as
// `-[NSURLSession dataTaskWithURL:completionHandler:]` (modelled in the
// previous diff in this stack), just different selectors.
//
// Cycle: `self -> _task -> task -> _completionHandler -> closure
//          -> captured self -> self`.
//
// Each BAD case closes a real retain cycle but Pulse currently has no
// model for the corresponding selector, so the cycle isn't visible at
// the `self.task = result` store. Hence the `_FN` suffixes on the BAD
// test functions; the next diff in the stack adds the matchers and
// drops the suffixes.

import Foundation

// BAD: `dataTask(with: URLRequest, completionHandler:)` overload.
final class URLSessionDataTaskRequestCycleHolder: @unchecked Sendable {
    var task: URLSessionDataTask?
    var label: String = ""

    func startBad(req: URLRequest) {
        task = URLSession.shared.dataTask(with: req) { _, _, _ in
            self.label = "received"
        }
    }
}

// GOOD: weak self capture — no cycle.
final class URLSessionDataTaskRequestWeakHolderGood: @unchecked Sendable {
    var task: URLSessionDataTask?
    var label: String = ""

    func startGood(req: URLRequest) {
        task = URLSession.shared.dataTask(with: req) { [weak self] _, _, _ in
            self?.label = "received"
        }
    }
}

// BAD: `downloadTask(with: URL, completionHandler:)` overload.
final class URLSessionDownloadTaskCycleHolder: @unchecked Sendable {
    var task: URLSessionDownloadTask?
    var label: String = ""

    func startBad(url: URL) {
        task = URLSession.shared.downloadTask(with: url) { _, _, _ in
            self.label = "received"
        }
    }
}

// GOOD: weak self capture — no cycle.
final class URLSessionDownloadTaskWeakHolderGood: @unchecked Sendable {
    var task: URLSessionDownloadTask?
    var label: String = ""

    func startGood(url: URL) {
        task = URLSession.shared.downloadTask(with: url) { [weak self] _, _, _ in
            self?.label = "received"
        }
    }
}

func test_url_session_data_task_request_self_capture_bad_FN(req: URLRequest) {
    let h = URLSessionDataTaskRequestCycleHolder()
    h.startBad(req: req)
}

func test_url_session_data_task_request_weak_self_capture_good(req: URLRequest) {
    let g = URLSessionDataTaskRequestWeakHolderGood()
    g.startGood(req: req)
}

func test_url_session_download_task_self_capture_bad_FN(url: URL) {
    let h = URLSessionDownloadTaskCycleHolder()
    h.startBad(url: url)
}

func test_url_session_download_task_weak_self_capture_good(url: URL) {
    let g = URLSessionDownloadTaskWeakHolderGood()
    g.startGood(url: url)
}
