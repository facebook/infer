/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>

int main() {
  int two;
  int equals_two;
  two = 2;
  equals_two = (two == 2);
  assert((equals_two != 0));
  assert((equals_two == 1));
}
