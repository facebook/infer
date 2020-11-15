/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <string>

namespace frontend {

namespace some {
namespace thing {
using foo_int = int;
int* bad_ptr() {
  int* p = new (int);
  delete p;
  return p;
}
} // namespace thing
} // namespace some

// test that NamespaceAliasDecl is handled correctly
void deref_null_namespace_alias_ptr_bad() {
  namespace st = some::thing;
  st::foo_int x = 0;
  int* p = st::bad_ptr();
  *p = x;
}

struct X {
  int f;
  X() {}
  X(int i) : f(i) {}
  X(X& from) : f(from.f) {}
  X(X&& from) : f(from.f) {}
  ~X() {}
  int get_f() const { return f; }
};

void construct_in_conditional_ok() {
  if (X(44).f != 44) {
    int* p = nullptr;
    *p = 42;
  }
}

bool is_zero(const X& x) { return x.get_f() == 0; }

void temp_passed_in_conditional_ok() {
  X x{44};
  if (is_zero(x)) {
    int* p = nullptr;
    *p = 42;
  }
}

void conditional_construction_xvalue_ok() {
  X x = true ? X(44) : X(33);
  if (x.f != 44) {
    int* p = nullptr;
    *p = 42;
  }
}

void conditional_construction_lvalue_ok() {
  const X& x = true ? X(44) : X(33);
  if (x.f != 44) {
    int* p = nullptr;
    *p = 42;
  }
}

void conditional_construction_int_lvalue_ok() {
  const int& x = true ? 44 : 33;
  if (x != 44) {
    int* p = nullptr;
    *p = 42;
  }
}

void conditional_construction_int_ptr_ok() {
  int* p = nullptr;
  int j = 44;
  int* x = true ? &j : p;
  int* y = true ? p : &j;
  if (*x != 44 || y != nullptr) {
    int* p = nullptr;
    *p = 42;
  }
}

int conditional_expression_bad(bool b) {
  bool ok = false;
  ok = ok && b;
  if (!ok) {
    int* p = nullptr;
    *p = 42;
  }
}

} // namespace frontend
