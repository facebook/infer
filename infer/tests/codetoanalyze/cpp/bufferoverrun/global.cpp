/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
static constexpr int ConstantGlobal[] = {5};

void access_constant_global_Bad() {
  int a[5];
  a[ConstantGlobal[0]] = 3;
}

void access_via_assignment_constant_global_Bad() {
  int a[5];
  const int* arr = ConstantGlobal;
  a[arr[0]] = 3;
}

static int StaticGlobal[][3] = {
    {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}};

void access_static_global1_Bad() { int* p = StaticGlobal[10]; }

void access_static_global2_Bad() { int a = StaticGlobal[0][10]; }

class Foo {

 public:
  int p;
  Foo(int x) { p = x; }
};

static const Foo ConstantGlobalFoos[] = {{8}};

void access_via_class_field_assignment_constant_global_Bad() {
  int a[5];
  const Foo foo = ConstantGlobalFoos[0];
  a[foo.p] = 3;
}

void access_via_class_field_constant_global_Bad() {
  int a[5];
  a[ConstantGlobalFoos[0].p] = 3;
}
