/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class X {
 public:
  X(int);
  X(const X &);
  X &operator=(const X &);
  ~X();
};
class Y {
 public:
  Y(int);
  ~Y();
};
X f(X);
Y g(Y);
void h() {
  X a(1);
  X b = f(X(2));
  Y c = g(Y(3));
  a = f(a);
}
