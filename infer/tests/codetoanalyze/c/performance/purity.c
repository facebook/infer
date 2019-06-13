/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
void (*fun_ptr)(int);
// just to sanity check that we don't fail on cost with purity analysis enabled
int loop(int k) {
  int p = 0;
  for (int i = 0; i < 1000; i++) {
    p = 3;
    (*fun_ptr)(10);
  }
  return p;
}
