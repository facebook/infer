// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// Static-helper-method-result stored in self, with the result holding back-ref
// to self via a delegate field. Pins the qualifier shape:
//
//   1) <Class>.<staticMethod>().<field>, assigned on line N
//   2) self->...
//
// The [<Class>.<staticMethod>()] half is the user-facing surface for Swift
// mangled call symbols — without the procname shrink it would show as raw
// [$s...] mangling and dominate the message text.

class Child {
    weak var parent: ParentVC?  // intentional FP setup happens via the delegate variant
}

class StrongChild {
    var owner: ParentVC?
}

class Helper {
    static func makeStrongChild() -> StrongChild {
        return StrongChild()
    }
}

class ParentVC {
    var heldChild: StrongChild?

    func wireUp_bad() {
        let c = Helper.makeStrongChild()
        c.owner = self
        self.heldChild = c
    }
}
