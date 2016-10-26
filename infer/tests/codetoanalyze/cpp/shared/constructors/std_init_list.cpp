/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
