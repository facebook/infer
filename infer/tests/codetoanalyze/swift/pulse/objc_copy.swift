// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// Coverage for the `-[NSObject copy]` model. The model uses
// `PulseOperations.shallow_copy` so the returned object inherits the
// receiver's field bindings, per-address attributes (including taint),
// and dynamic-type constraints.
//
// The semantic teeth for this model live in
// `infer/tests/codetoanalyze/objc/pulse/taint/StoreIvarTaint.m`: a
// taint flow through `[accessToken copy]` is preserved (the existing
// `TAINT_ERROR` continues to fire) and the trace no longer carries
// the noisy `in call to NSObject.copy with no summary` step. The
// canonical Swift pattern below pins the dispatch direction: a method
// call on the copy resolves through the model's recovered dynamic
// type rather than falling back to unknown.

import Foundation

@objc class CopyableThing: NSObject {
    @objc @inline(never) func reading() -> Int {
        return 7
    }
}

func test_copy_dispatch_bad() {
    let original = CopyableThing()
    let dup = original.copy() as! CopyableThing
    assert(dup.reading() != 7) // true positive: reading() returns 7
}

func test_copy_dispatch_good() {
    let original = CopyableThing()
    let dup = original.copy() as! CopyableThing
    assert(dup.reading() == 7) // true negative: reading() returns 7
}
