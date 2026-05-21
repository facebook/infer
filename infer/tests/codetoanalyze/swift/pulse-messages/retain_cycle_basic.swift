// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// Plain ivar <-> ivar retain cycle. Pins the most common message shape:
//
//   1) a->b, assigned on line N
//   2) b->a, assigned on line N
//
// All field references in the qualifier should be human-readable property names,
// not Swift mangled-symbol fragments.

class Node {
    var partner: Node?
}

func setPartners_bad(_ x: Node, _ y: Node) {
    x.partner = y
    y.partner = x
}
