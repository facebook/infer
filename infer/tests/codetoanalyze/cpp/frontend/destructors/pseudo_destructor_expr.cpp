/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
