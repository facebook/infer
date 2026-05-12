// Coverage for retain cycles that close through a closure stored on
// `self` and capturing `self`. Companion to the delegate-based RC
// fixtures (rc_*delegate*.swift): same root-cause shape but the
// strong-back edge is a captured `self` reference inside a stored
// closure rather than a property assignment.
//
// All `_bad` cases here are detected today. The `_bad_FN` cases are
// known false negatives — they hit when the closure-storing object
// is built and stored by an unmodelled function (Timer / Combine
// `.sink` / NotificationCenter / DispatchSourceTimer …), so Pulse
// can't see that the closure is retained by something the caller
// then stores on `self`.

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

// FN: closure passed to an unmodelled framework function whose
// returned holder is stored on `self`. Closes the cycle as
// `self -> token -> [opaque framework holder] -> closure -> self`.
// Pulse has no summary for `externalRegisterHandler` so it doesn't
// model that the returned holder retains the closure. Real-world:
// `self.timer = Timer.scheduledTimer(...) { ... self.tick() }`.
class FrameworkUser {
    var token: AnyObject?
    var label: String = ""

    func setupBadFramework_FN() {
        token = externalRegisterHandler { self.label = "fired" }
    }
}

// FN: same shape, but the holder is stashed inside a Set-like
// wrapper on `self`, mirroring Combine's `.store(in: &cancellables)`
// idiom. The extra hop through the wrapper doesn't change the
// underlying gap — Pulse still doesn't know the framework holder
// retains the closure.
class CancellableLikeWrapper {
    var inner: AnyObject?
}

class FrameworkUserStoreIn {
    var cancellables: CancellableLikeWrapper = CancellableLikeWrapper()
    var label: String = ""

    func setupBadFrameworkStoreIn_FN() {
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

func test_closure_framework_bad_FN() {
    let f = FrameworkUser()
    f.setupBadFramework_FN()
}

func test_closure_framework_store_in_bad_FN() {
    let f = FrameworkUserStoreIn()
    f.setupBadFrameworkStoreIn_FN()
}

func test_closure_weak_self_capture_good() {
    let g = WeakCaptureGood()
    g.setupGood()
}
