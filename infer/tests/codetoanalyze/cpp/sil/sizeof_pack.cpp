/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

template <typename... Ts>
int count_args(const Ts&... ts) {
  if (sizeof...(ts) == 0) {
    return -1;
  }
  return sizeof...(ts);
}

int test_sizeof_pack() { return count_args(1, 2, 3); }
