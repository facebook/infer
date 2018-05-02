/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

void nop() { int x = 0; }

void do_n_times(int n) {
  for (int i = 0; i < n; i++) {
    nop();
  }
}

void do_2_times_Good() { do_n_times(2); }

void do_2K_times_Bad() { do_n_times(2000); }
