/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import Foundation

// A class declared *local to a function* gets a Swift mangled name whose
// suffix (`...L_C`) the retain-cycle reporter's demangler does not yet
// recognise, so the cycle message currently prints the raw mangled name
// (`object of type class T5Hello...L_C`) instead of `LocalNode`. This
// fixture pins that behaviour; a demangler fix should turn the label into
// the plain class name.
func makeLocalClassCycle() {
  final class LocalNode {
    var onEvent: (() -> Void)?

    func wireBad() {
      onEvent = { self.fire() }
      _ = onEvent
    }

    func fire() {}
  }

  let node = LocalNode()
  node.wireBad()
}
