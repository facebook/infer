/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <iostream>
#include <vector>

void FN_deref_vector_element_after_lifetime_bad() {
  std::vector<int> x = {0, 0};
  int* y = &x[1];
  x.push_back(4);
  std::cout << *y << "\n";
}

int main() { FN_deref_vector_element_after_lifetime_bad(); }
