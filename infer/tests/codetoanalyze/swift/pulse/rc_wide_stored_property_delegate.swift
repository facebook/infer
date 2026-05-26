// Pulse retain-cycle detection on a class with a non-pointer-sized stored
// property BEFORE the strong delegate. swiftc -O lays out:
//   - swift refcounted header (16 bytes)
//   - frame: CGRect (32 bytes; 4 doubles)
//   - delegate: existential container (16 bytes)
// so delegate lands at byte offset 48. The byte-offset GEP resolver in
// Llair2TextualField.lookup_field_by_byte_offset now consults a
// (class, byte_offset) → field_name map built from Wvd field-offset descriptor
// globals, so the delegate field at offset 48 resolves to its true name and the
// inlined-setter store reaches Pulse's heap. Without that map, the size
// estimator assumed 8 bytes for any non-existential field and computed the
// delegate offset as 24 → unresolved → [llvm_nondet] → cycle dropped.
//
// Companion case `rc_inline_concrete_delegate.swift` is the simpler shape
// (single existential, no leading wide property).

import CoreGraphics

protocol WideLayoutDelegate: AnyObject {
    func didTap()
}

class WideHeaderView {
    var frame: CGRect = .zero
    var delegate: WideLayoutDelegate?
    init() {}
}

class WideHeaderController {

    private lazy var header: WideHeaderView = {
        let h = WideHeaderView()
        h.delegate = self
        return h
    }()

    func loadView() {
        _ = header
    }
}

extension WideHeaderController: WideLayoutDelegate {
    func didTap() {}
}

func test_wide_header_delegate_bad() {
    let vc = WideHeaderController()
    vc.loadView()
}
