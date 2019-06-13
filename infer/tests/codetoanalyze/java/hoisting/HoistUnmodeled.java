/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class HoistUnmodeled {

  // Any unmodeled (e.g. timing) call is assumed to be modifying global
  // state
  void timing_calls_dont_hoist() {
    for (int i = 0; i < 10; i++) {
      System.nanoTime();
    }
  }

  // doesn't read from global state or call any unmodeled function, a
  // harmless pure function
  void harmless_pure() {}

  // It should be ok to hoist harmless_pure() since it doesn't read
  // from global state.
  void harmless_hoist_FN() {
    for (int i = 0; i < 10; i++) {
      timing_calls_dont_hoist(); // don't hoist
      harmless_pure(); // ok to hoist
    }
  }
}
