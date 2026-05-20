// Coverage for retain-cycle attribution when the cycle-closing edge
// lives inside compiler-generated SIL (closure thunks, partial-apply
// forwarders, witness thunks, overlay-init, async continuations).
//
// Synthetic SIL is captured under a per-bitcode
// `<bitcode_id>:compiler-generated.sil` sentinel rather than rebound to
// an arbitrary user `.swift`. The headline `location` of a
// `RETAIN_CYCLE` whose `Trace.get_outer_location` lands on such a
// sentinel falls back to the analyzed proc's site (cycle-detection
// store / call) so the report points at a real user file.

// =====================================================================
// Fixture 1 — user frame adjacent to a synthetic edge.
// =====================================================================
//
// Expected `RETAIN_CYCLE` headline: this file, on the
// `self.onTick = { ... }` assignment in `NodeWithClosure.init` (NOT
// `<bitcode_id>:compiler-generated:0:0`).

final class NodeWithClosure {
    var onTick: (() -> Void)?

    init() {
        // CAPTURE SITE — headline should land here.
        self.onTick = {
            self.handle()
        }
    }

    func handle() {}
}

func test_compiler_generated_user_adjacent_bad() {
    _ = NodeWithClosure()
}

// =====================================================================
// Fixture 2 — all-synthetic cycle (witness-thunk-only).
// =====================================================================
//
// The cycle `a <-> b` is closed entirely through the witness thunk for
// `Tickable.tick` (default impl in the protocol extension) plus the
// partial-apply forwarder it dispatches through. Every retain edge on
// the cycle path lives in synthesized frames.
//
// Expected `RETAIN_CYCLE` headline: this file, on the second
// `.peer =` assignment in
// `test_compiler_generated_all_synthetic_suppress` — the
// cycle-detection site. Strictly better than
// `<compiler-generated>:0:0` even though no user code is on the cycle
// path itself.

protocol Tickable: AnyObject {
    var peer: Tickable? { get set }
    func tick()
}

extension Tickable {
    // Lowered to a witness thunk that retains `self` and forwards to
    // `peer`. Strong edges on the cycle live inside this thunk and the
    // partial-apply forwarder it dispatches through.
    func tick() {
        self.peer?.tick()
    }
}

final class TickableA: Tickable { var peer: Tickable? }
final class TickableB: Tickable { var peer: Tickable? }

func test_compiler_generated_all_synthetic_suppress() {
    let a = TickableA()
    let b = TickableB()
    a.peer = b
    b.peer = a  // cycle, but every retain edge on the path is synthetic
}
