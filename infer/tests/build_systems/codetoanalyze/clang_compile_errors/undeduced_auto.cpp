/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Each use of UNDECLARED is a compile error. Clang still produces an AST, in
// which a variable declared `auto` with such an initializer keeps an undeduced
// type.

struct S {
  int x;
};

struct Owner {
  int* p;
  ~Owner() { delete p; }
};

int get(int);
int& getref(int);

int function_scope_exit_bad() {
  S* p = nullptr;
  auto ret = get(UNDECLARED);
  if (ret) {
    return ret;
  }
  return p->x;
}

void block_scope_exit_bad() {
  S* p = nullptr;
  {
    const auto ret = get(UNDECLARED);
    (void)ret;
  }
  p->x = 1;
}

void loop_scope_exit_bad(int n) {
  S* p = nullptr;
  for (int i = 0; i < n; i++) {
    auto ret = get(UNDECLARED);
    if (ret) {
      break;
    }
  }
  p->x = 1;
}

// the type of `ret` is a ParenType whose desugared type is the undeduced `auto`
void parenthesized_declarator_bad() {
  S* p = nullptr;
  {
    auto(ret) = get(UNDECLARED);
    (void)ret;
  }
  p->x = 1;
}

void decltype_of_undeduced_bad() {
  S* p = nullptr;
  auto ret = get(UNDECLARED);
  {
    decltype(ret) y = 1;
    (void)y;
  }
  p->x = 1;
}

int other_destructors_still_called_bad() {
  int* q = new int(0);
  {
    Owner o{q};
    auto ret = get(UNDECLARED);
    (void)ret;
  }
  return *q;
}

// the hidden variable that holds the decomposed object keeps an undeduced type
void decomposition_scope_exit_bad() {
  S* p = nullptr;
  {
    auto [lo, hi] = get(UNDECLARED);
  }
  p->x = 1;
}

int function_scope_exit_ok() {
  S s{0};
  S* p = &s;
  auto ret = get(UNDECLARED);
  if (ret) {
    return ret;
  }
  return p->x;
}

// clang drops the uses of the invalid `ret` from the AST
int dropped_uses_ok() {
  auto ret = get(UNDECLARED);
  if (ret < 0) {
    return ret;
  }
  return 0;
}

void reference_to_undeduced_ok() {
  auto& r = getref(UNDECLARED);
  (void)r;
}

int explicit_type_bad() {
  S* p = nullptr;
  int ret = get(UNDECLARED);
  if (ret) {
    return ret;
  }
  return p->x;
}
