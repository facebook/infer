/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <initializer_list>

class X {
 public:
  X(std::initializer_list<int> list) {
    for (auto i = list.begin(); i != list.end(); i++) {
      sum = sum + *i;
    }
  }

 private:
  int sum;
};

int main() { X x = {1, 2, 3, 4, 5}; }
