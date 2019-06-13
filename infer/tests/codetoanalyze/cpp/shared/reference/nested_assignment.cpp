/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void normal() {
  int a = 3;
  int& ref_from_val = a;
  int& ref_from_ref = ref_from_val;
}

void nested() {
  int a = 3;
  int& ref_from_val = a = 4;
  int& ref_from_ref = ref_from_val = 6;
}

void crazy_nested() {
  int a = 3;
  int b = a;

  // a will refer to same object as ref_from_val and ref_from_ref, but different
  // than b
  int& ref_from_val = a = b = 4;
  int& ref_from_ref = ref_from_val = b = 5;
}
