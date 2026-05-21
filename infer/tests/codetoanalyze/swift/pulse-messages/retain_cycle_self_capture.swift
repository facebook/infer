// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// Self-capture-in-closure retain cycle. Pins the message shape that mentions a
// closure value: the closure's tuple field appears via [swift_allocObject().__infer_tuple_field_N]
// on one side of the cycle, and a strong property holds the closure on the other.
//
// This is the shape that surfaced the original Swift mangled-name readability
// complaint — a long [$s...] symbol on the call site dominated the qualifier text.

class Holder {
    var onAction: (() -> Void)?

    func setupBad_bad() {
        onAction = {
            self.doWork()
        }
    }

    func doWork() {}
}
