/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

bool always_ret_false() { return false; }

void linear_loop(int x) {
  for (int i = 0; i < x; i++) {
  }
}

void unreachable_branch_constant(int x) {
  if (always_ret_false()) {
    linear_loop(x); // this is unreachable
  }
}

void unreachable_branch_constant2(int x) {
  if (always_ret_false()) {
    // the following nodes are unreachable
    for (int i = 0; i < x; i++) {
    }
  }
}

void unreachable_branch_linear_loop(int x) {
  if (always_ret_false()) {
    linear_loop(x); // this is unreachable
  }
  linear_loop(x);
}
