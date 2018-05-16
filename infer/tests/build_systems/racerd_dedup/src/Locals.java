/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import javax.annotation.concurrent.ThreadSafe;

@ThreadSafe
public class Locals {
  int f;

  static Locals id(Locals o) { return o; }

  static void FN_raceOnTemporary_bad(Locals o) {
    id(o).f = 5;
  }
}
