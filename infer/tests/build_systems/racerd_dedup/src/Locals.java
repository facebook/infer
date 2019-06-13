/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import javax.annotation.concurrent.ThreadSafe;

@ThreadSafe
public class Locals {
  int f;

  static Locals id(Locals o) {
    return o;
  }

  static void FN_raceOnTemporary_bad(Locals o) {
    id(o).f = 5;
  }
}
