/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void apply(void (*f)(int*), int* p) { (*f)(p); }

int call_apply() {
  int x = 1;
  return x;
}
