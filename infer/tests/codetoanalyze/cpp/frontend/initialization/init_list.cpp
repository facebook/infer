/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace init_list {
struct X {
  int a;
  int* p;
};
struct Y { // POD struct
  int z;
  X x;
};

struct C { // non-POD struct
  int z;
  X x{};
  virtual void f() {} // this make C non-POD
  C() = default;
  C(int a, int b, const X& x) : z(a + b), x(x) {}
};

void zero_init_primitive() {
  int i{};
  int* p{};
  int* p2{nullptr};
  float f{};
}

void zero_init_record() {
  Y y{};
  C c{}; // this will call default constructor
}

void record_init() {
  X x{1, nullptr};
  Y y1{1, x}; // x will be copied
  Y y2{1, {2, nullptr}};

  C c{1, 2, x}; // this will call C constructor
}

void list_init() {
  int ti[4] = {1, 2};
  Y y;
  Y& yref = y;
  Y ty[3] = {{1, {2, nullptr}}, y, yref};
}

void init_in_binop(int x) { x = -x & ~int{0}; }

} // namespace init_list
