/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

typedef int T;
int f(int* p) {
  int x = *p;
  p->T::~T();
  return x;
}

template <typename T>
int destroy(T* ptr) {
  ptr->T::~T();
  return 0;
}

void test() {
  int* t = 0;
  destroy<int*>(&t);
}
