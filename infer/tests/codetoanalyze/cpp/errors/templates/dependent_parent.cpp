/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
struct Y {
  int y();
};

template <typename T>
struct X {
  struct Y;
  int x_y() {
    struct Y y;
    return y.y();
  }
};

template <typename T>
struct Z {
  int z() { return 3; };
};

template <typename T>
struct X<T>::Y : Z<T> {
  int y() { return Y::z() - 3; }
};

void instantiate_class_bad() {
  struct X<int> x;
  int n = 1 / x.x_y();
}
