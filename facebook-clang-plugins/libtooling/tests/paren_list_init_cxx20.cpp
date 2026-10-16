/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct pod {
  int a;
  int b;
};

void test() {
  pod p(1);
  int i_a[3](1, 2);
  auto *p_a = new pod[2](pod(1, 2));
}
