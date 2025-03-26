/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
public class Latent {
  // Without --topl-filter-unsure, this should be a latent issue marked as PULSE_MANIFEST
  static void test_Ok(int x) {
    g(x);
  }

  // Without --topl-filter-unsure, this should remain a latent issue, marked as TOPL_MANIFEST
  static void test_Latent(N p) {
    N old_p = p;
    while (p != null) {
      p = p.next;
      if (old_p == p) {
        g(42);
        break;
      }
    }
  }

  static void g(int _x) {}
}

class N {
  int data;
  N next;
}
