/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int conditionalBasicOK() {
  int x = 5;
  x = x ? x : 1;
  return x;
}

int FP_conditionalOpaqueOk() {
  int x = 5;
  x = x ?: 1;
  return x;
}
