/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void stat_cast() {
  int a;
  long long la = static_cast<long long>(a);
}

void functional_cast() { int a = int(2 + 3.4); }
