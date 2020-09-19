/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

template <class T>
T get(T x) {
  return x;
}

// specialization
template <>
int get(int x) {
  return 2 * x;
}

// explicit instantiacion
template double get(double x);

void test() {
  char c;
  float f;
  c = get(c);
  f = get(f);
}
