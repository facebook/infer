/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class EvilCfg {
  public void foo_FP(int i, int j, boolean b) {
    int k, l, m, n;

    k = b ? i : j;
    l = b ? k : i;
    m = b ? k : l;
    n = b ? m : k;
    for (; n < 10; n++) {}

    k = b ? i : j;
    l = b ? k : i;
    m = b ? k : l;
    n = b ? m : k;
    for (; n < 10; n++) {}

    k = b ? i : j;
    l = b ? k : i;
    m = b ? k : l;
    n = b ? m : k;
    for (; n < 10; n++) {}

    k = b ? i : j;
    l = b ? k : i;
    m = b ? k : l;
    n = b ? m : k;
    for (; n < 10; n++) {}
  }
}
