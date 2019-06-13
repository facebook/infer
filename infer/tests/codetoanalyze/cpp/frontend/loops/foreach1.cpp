/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// iterator
struct iterator {
  int val;
  iterator operator++() {
    val += 1;
    return *this;
  }
  int operator*() {
    return val;
  } // this should return type of values stored in vec
};
bool operator!=(iterator i1, iterator i2) { return i1.val != i2.val; }

struct vec {
  vec(int size) {
    begin_.val = 0;
    end_.val = size;
  }
  iterator begin() { return begin_; }
  iterator end() { return end_; }

  iterator begin_;
  iterator end_;
};

void test() {
  vec vector(10);
  for (int value : vector) {
    int temp = value * value + 10;
  }
}
