import Foundation

@objc class ObjCClass : NSObject {
    override init() {
        super.init()
    }

    // @objc dynamic forces the Swift compiler to dispatch through the
    // Objective-C runtime, which lowers to objc_msgSend(receiver, selector, ...)
    // rather than a Swift vtable / direct call. This is the shape that surfaces
    // the prod "function objc_msgSend called with N arguments while declared
    // with 0 parameters" verifier error.
    @objc dynamic var value: Int = 0
}

// 2-argument objc_msgSend: dynamic getter — emits objc_msgSend(c, sel("value"))
func getValueViaObjC(_ c: ObjCClass) -> Int {
    return c.value
}

// 3-argument objc_msgSend: dynamic setter — emits objc_msgSend(c, sel("setValue:"), v)
func setValueViaObjC(_ c: ObjCClass, _ v: Int) {
    c.value = v
}

// Runtime-computed selector via NSObject.perform / responds(to:) — the
// selector reg cannot be tracked back to a literal by ProcState.find_selector,
// so resolve_objc_msgSend falls through to the dynamic path that emits
// `$builtins.objc_msgSend(receiver, selector, ...)`. This is the shape that
// reproduces the prod "function objc_msgSend called with N arguments while
// declared with 0 parameters" verifier error (e.g. METAHDRImageView.swift:264,
// BCNCommentBubbleAnimator.swift:576).
func dispatchDynamicSelector(_ obj: NSObject, _ s: Selector) -> Bool {
    // 3-arg objc_msgSend(obj, sel("respondsToSelector:"), s)
    return obj.responds(to: s)
}

func performDynamicSelector(_ obj: NSObject, _ s: Selector) -> Unmanaged<AnyObject>? {
    // 2-arg objc_msgSend(obj, s) where s is a runtime selector
    return obj.perform(s)
}
