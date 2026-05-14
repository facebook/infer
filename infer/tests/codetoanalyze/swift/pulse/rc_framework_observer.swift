// Coverage for retain cycles via Apple framework APIs that retain
// closures capturing `self`. Companion to `rc_framework_timer.swift`;
// same shape (`framework retains a block, the block captures self,
// the user stores the framework-returned token on self`), different
// underlying ObjC procname.

import Foundation

// BAD: NotificationCenter.addObserver(forName:object:queue:using:)
// retains the closure on the returned observer token; the token is
// stored on `self.observer`, closing
// `self -> observer -> block -> captured self -> self`.
final class NotifCycleHolder: @unchecked Sendable {
    var observer: NSObjectProtocol?
    var label: String = ""

    func startBad() {
        observer = NotificationCenter.default.addObserver(
            forName: Notification.Name("Tick"), object: nil, queue: nil
        ) { _ in
            self.label = "fired"
        }
    }
}

// GOOD: weak `self` capture in the observer block — no cycle.
final class NotifWeakHolderGood: @unchecked Sendable {
    var observer: NSObjectProtocol?
    var label: String = ""

    func startGood() {
        observer = NotificationCenter.default.addObserver(
            forName: Notification.Name("Tick"), object: nil, queue: nil
        ) { [weak self] _ in
            self?.label = "fired"
        }
    }
}

func test_notif_self_capture_bad() {
    let h = NotifCycleHolder()
    h.startBad()
}

func test_notif_weak_self_capture_good() {
    let g = NotifWeakHolderGood()
    g.startGood()
}
