/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void trivial_no_condition_bad() {
  for(;;);
}

void trivial_constant_true_bad() {
  while(0 == 0) {}
}

void condition_never_changes_bad() {
  int i = 0;
  int j = 0;
  while (i == 0) {
    j++;
  }
}

void FN_never_zero_bad() {
  int i = 1;
  while (i != 0) {
    // FN because of abstraction: i could be < 0
    i++;
  }
}

void always_gt_zero_bad() {
  int i = 1;
  while (i > 0) {
    i++;
  }
}

void complex_condition_always_true_bad() {
  int i = 0;
  while (i+2 > 0) {
    i++;
  }
}
