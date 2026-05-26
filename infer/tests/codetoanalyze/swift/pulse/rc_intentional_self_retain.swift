// Coverage for the `pulse-retain-cycle-blocklist-pattern` config flag,
// enabled for Swift in this Makefile with the same `.*[Rr]etain.*` regex
// `fbobjc/.inferconfig` uses for ObjC code.
//
// Pattern: code that intentionally retains `self` (typically via
// `self.retainedSelf = self` or `self.selfRetain = self`) to keep itself
// alive across an async boundary, and clears the slot in some completion
// path. These cycles are technically real — Pulse correctly detects the
// strong back-edge — but they're deliberate, and the naming convention
// (anything matching `[Rr]etain`) signals the author knows what they're
// doing. ObjC code has used this filter for years; this fixture pins the
// equivalent behaviour for Swift now that the `Language.Swift` short-
// circuit in `PulseRetainCycleChecker.should_report_cycle` is removed.
//
// In production: 5 unique cycles / 104 instances over a 72h sample
// (BCNDearAlgo[Edit]?SubmissionRequest, FBShortsChapterAttachment[Feedback]?
// Bottomsheet, IGMainFeedBackgroundPrefetchCoordinator) all flipped from
// `RETAIN_CYCLE` to suppressed by this filter.

private final class IntentionalSelfRetain {
    // Intentional self-retain across the async work. Matches the regex →
    // BLOCKLISTED, no RETAIN_CYCLE expected.
    private var retainedSelf: IntentionalSelfRetain?

    func fire_good_FP() {
        // The classic "I'll deliberately leak myself until the work is
        // done" pattern: store self into a self-named slot. The matching
        // cleanup [self.retainedSelf = nil] in some completion path is
        // omitted from this fixture only because [DispatchQueue.global().
        // async { [weak self] in self?.retainedSelf = nil }] trips Swift's
        // strict-concurrency sendability check, which is orthogonal to
        // what we're testing here. The blocklist filter fires on the
        // property name regardless of whether a cleanup is wired up.
        self.retainedSelf = self
    }
}

private final class LeakedSelfNotMatched {
    // Same shape but property name does NOT contain `retain` → blocklist
    // does NOT fire → RETAIN_CYCLE expected. Pins the negative direction
    // so a future overly-broad blocklist rewrite would change this
    // baseline.
    private var leakedSelf: LeakedSelfNotMatched?

    func fire_bad() {
        self.leakedSelf = self
    }
}

public func test_intentional_self_retain_no_report_good_FP() {
    let r = IntentionalSelfRetain()
    r.fire_good_FP()
}

public func test_leaked_self_not_blocklisted_bad() {
    let l = LeakedSelfNotMatched()
    l.fire_bad()
}
