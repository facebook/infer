/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <map>

void test() {
  std::map<int, int> m;
  auto itr1 = m.begin();
  auto itr2 = itr1;
}
