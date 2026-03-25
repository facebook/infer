/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int live_across_call();

int call_then_use() {
  int x = live_across_call();
  return x + 1;
}
