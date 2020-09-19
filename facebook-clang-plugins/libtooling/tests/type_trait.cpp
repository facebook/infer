/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct X {
  virtual void f() {}
};

template <class T>
bool is_pod() {
  return __is_pod(T);
}

bool a = is_pod<int>();
bool b = is_pod<X>();
