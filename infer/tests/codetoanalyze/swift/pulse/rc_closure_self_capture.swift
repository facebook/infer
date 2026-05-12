// Coverage for retain cycles that close through a closure stored on
// `self` and capturing `self`. Companion to the delegate-based RC
// fixtures (rc_*delegate*.swift): same root-cause shape but the
// strong-back edge is a captured `self` reference inside a stored
// closure rather than a property assignment.
//
// All `_bad` cases here are detected. The framework-mediated
// variants (closure passed to an unmodelled function whose returned
// holder is stored on `self`) are caught by the
// `external_register_handler` Pulse model in `PulseModelsSwift`,
// which stands in for the canonical Swift framework APIs that follow
// this shape (Timer / Combine `.sink` / NotificationCenter /
// DispatchSourceTimer …). The model establishes the
// `holder->captured_env` edge so the retain-cycle checker can close
// `self -> token -> holder -> captured_env -> self`.

class Handler {
    var onAction: (() -> Void)?
    var label: String = ""

    // BAD: direct property assignment inside a method.
    func setupBadDirect() {
        onAction = { self.label = "fired" }
    }

    // BAD: assigned in init.
    init() {
        onAction = { self.label = "fired in init" }
    }

    // BAD: closure stored on `self` indirectly via a helper method.
    func setupBadViaHelper() {
        store({ self.label = "fired via helper" })
    }

    private func store(_ block: @escaping () -> Void) {
        onAction = block
    }
}

// BAD: closure stored on a child object owned by `self`. Closes the
// cycle as `self -> child -> closure -> self`. Pulse traces through
// the typed field stores and detects it.
class ParentWithChild {
    var label: String = ""
    let child: ChildHolder

    init() {
        let c = ChildHolder()
        self.child = c
        c.callback = { self.label = "fired" }
    }
}

class ChildHolder {
    var callback: (() -> Void)?
}

// Unmodelled "framework" function: takes a closure, returns an opaque
// holder that retains it. Pulse has no summary for this and can't see
// that the closure is stored on the returned object. Mimics the
// shape of:
//   - `Timer.scheduledTimer(withTimeInterval:..., block:)` -> Timer
//   - `NotificationCenter.addObserver(forName:...,using:)` -> NSObjectProtocol
//   - `Publisher.sink {...}` -> Cancellable / `.store(in: &cancellables)`
//   - `DispatchSourceTimer.setEventHandler {...}` (slightly different
//     shape, same root cause).
@_silgen_name("external_register_handler")
func externalRegisterHandler(_ callback: @escaping () -> Void) -> AnyObject

// BAD: closure passed to a framework-shaped function whose returned
// holder is stored on `self`. Closes the cycle as
// `self -> token -> holder -> captured_env -> self`. Detected via
// the `external_register_handler` model in `PulseModelsSwift`, which
// stands in for `Timer.scheduledTimer(...) { ... self.tick() }` and
// similarly-shaped framework APIs.
class FrameworkUser {
    var token: AnyObject?
    var label: String = ""

    func setupBadFramework() {
        token = externalRegisterHandler { self.label = "fired" }
    }
}

// BAD: same shape, but the holder is stashed inside a Set-like
// wrapper on `self`, mirroring Combine's `.store(in: &cancellables)`
// idiom. The extra hop through the wrapper doesn't change the
// underlying cycle path; the model still establishes the
// `holder->captured_env` edge.
class CancellableLikeWrapper {
    var inner: AnyObject?
}

class FrameworkUserStoreIn {
    var cancellables: CancellableLikeWrapper = CancellableLikeWrapper()
    var label: String = ""

    func setupBadFrameworkStoreIn() {
        let token = externalRegisterHandler { self.label = "fired" }
        cancellables.inner = token
    }
}

// GOOD: weak capture, no cycle.
class WeakCaptureGood {
    var onAction: (() -> Void)?
    var label: String = ""

    func setupGood() {
        onAction = { [weak self] in self?.label = "fired" }
    }
}

func test_closure_direct_bad() {
    let h = Handler()
    h.setupBadDirect()
}

func test_closure_init_bad() {
    let _ = Handler()
}

func test_closure_via_helper_bad() {
    let h = Handler()
    h.setupBadViaHelper()
}

func test_closure_via_child_bad() {
    let _ = ParentWithChild()
}

func test_closure_framework_bad() {
    let f = FrameworkUser()
    f.setupBadFramework()
}

func test_closure_framework_store_in_bad() {
    let f = FrameworkUserStoreIn()
    f.setupBadFrameworkStoreIn()
}

func test_closure_weak_self_capture_good() {
    let g = WeakCaptureGood()
    g.setupGood()
}
