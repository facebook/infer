/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

enum ENUM { val1, val2 };

template <class T>
T get() {
  return T();
}

void test() {
  int i = get<int>();
  float f = get<float>();
  float* fp = get<float*>();
  get<void>();
  ENUM x = get<ENUM>();
  float f2 = float();
}
