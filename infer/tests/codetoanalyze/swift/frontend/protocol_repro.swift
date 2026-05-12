// FOCUS: regression test for the Llair2Textual `Ap2 (Update idx, ...)`
// reverse-execution-order fix.
//
// The synthesized ObjC bridge for `translationInWindow.get` (the @objc
// protocol getter returning a CGPoint) emits a Llair `Store` whose value
// is a nested `Update` tree. Before the fix, the translator returned the
// inner store/load instructions in forward-execution order, but the
// `cmnd_to_instrs` Store handler accumulates them in reverse-execution
// order (`List.rev`'d at the end of the block). The result was that the
// inner stores were emitted BEFORE the loads that produced their values,
// tripping the textual verifier with "ident is read before being written"
// for the synthesized bridge.

import Foundation

@objc public protocol GestureProvider: NSObjectProtocol {
    var translationInWindow: CGPoint { get }
    func analyticsExtras() -> [String: any NSObjectProtocol]
}

@objc public final class SwipeInfo: NSObject, GestureProvider {
    public var translationInWindow: CGPoint { CGPoint(x: 0, y: 0) }
    public func analyticsExtras() -> [String: any NSObjectProtocol] {
        return [:]
    }
}
