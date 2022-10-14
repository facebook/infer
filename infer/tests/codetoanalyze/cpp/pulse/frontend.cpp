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

void conditional_expression_bad(bool b) {
  bool ok = false;
  ok = ok && b;
  if (!ok) {
    int* p = nullptr;
    *p = 42;
  }
}

class Frontend {
 public:
  int a;

  Frontend(int n) { a = (n == 0) ? 0 : 1; }

  Frontend(Frontend x, int n) {
    x.a = (n == 0) ? 0 : 1;
    a = x.a;
  }

  void set_field_via_local(int n) {
    int* b = (int*)malloc(sizeof(int));
    if (b) {
      *b = 0;
      *b = (n == 0) ? 0 : 1;
      a = *b;
      free(b);
    } else {
      a = (n == 0) ? 0 : 1;
    }
  }
};

void call_Frontend_constructor_ok() {
  Frontend x = Frontend(10); // x.a is 1
  if (x.a != 1) {
    int* p = nullptr;
    *p = 42;
  }
}

void call_Frontend_constructor_bad() {
  Frontend x = Frontend(10); // x.a is 1
  if (x.a == 1) {
    int* p = nullptr;
    *p = 42;
  }
}

void call_Frontend_constructor2_ok() {
  Frontend x = Frontend(0); // x.a is 0
  Frontend y = Frontend(x, 10); // y.a is 1
  if (y.a != 1) {
    int* p = nullptr;
    *p = 42;
  }
}

void call_Frontend_constructor2_bad() {
  Frontend x = Frontend(0); // x.a is 0
  Frontend y = Frontend(x, 10); // y.a is 1
  if (y.a == 1) {
    int* p = nullptr;
    *p = 42;
  }
}

void call_set_field_via_local_ok() {
  Frontend x = Frontend(0); // x.a is 0
  x.set_field_via_local(10); // x.a is 1
  if (x.a != 1) {
    int* p = nullptr;
    *p = 42;
  }
}

void call_set_field_via_local_bad() {
  Frontend x = Frontend(0); // x.a is 0
  x.set_field_via_local(10); // x.a is 1
  if (x.a == 1) {
    int* p = nullptr;
    *p = 42;
  }
}

void not_boolean_ok() {
  bool t = true;
  bool* b = (bool*)malloc(sizeof(bool));
  if (b) {
    *b = true;
    *b = !t; // *b is false
    if (*b) {
      int* p = nullptr;
      *p = 42;
    }
    free(b);
  }
}

void not_boolean_bad() {
  bool f = false;
  bool* b = (bool*)malloc(sizeof(bool));
  if (b) {
    *b = false;
    *b = !f; // *b is true
    if (*b) {
      int* p = nullptr;
      *p = 42;
    }
    free(b);
  }
}

struct double_fields_struct {
  int v;
  int a;
};

double_fields_struct get_double_fields_struct() {
  double_fields_struct b;
  b.v = 42;
  b.a = 42;
  return b;
}

void init_double_fields_struct_ok() {
  double_fields_struct y{get_double_fields_struct()};
  if (y.v != 42) {
    int* p = nullptr;
    *p = 42;
  }
}

struct single_field_struct {
  int v;
};

single_field_struct get_single_field_struct() {
  single_field_struct b;
  b.v = 42;
  return b;
}

void FP_init_single_field_struct_ok() {
  single_field_struct y{get_single_field_struct()};
  if (y.v != 42) {
    int* p = nullptr;
    *p = 42;
  }
}

struct Base {
  Base(int* p1, int* p2) { *p1 = 42; }
};

struct ForwardConstructorParams : Base {
  // check that the parameters are forwarded in the correct order by the
  // frontend
  using Base::Base;
};

void derived_constructor_forwards_param_ok() {
  int x;
  ForwardConstructorParams A{&x, NULL};
}

} // namespace frontend
