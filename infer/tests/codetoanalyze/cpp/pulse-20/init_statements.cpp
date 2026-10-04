/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace init_statements {

struct S {
  int x;
};

struct Owner {
  int* p;
  Owner() : p(new int(0)) {}
  ~Owner() { delete p; }
};

int range_for_init_null_deref_bad() {
  int arr[3] = {1, 2, 3};
  int sum = 0;
  for (S* p = nullptr; int v : arr) {
    sum += v + p->x;
  }
  return sum;
}

int range_for_null_deref_in_init_bad() {
  int arr[3] = {1, 2, 3};
  S* q = nullptr;
  int sum = 0;
  for (int base = q->x; int v : arr) {
    sum += base + v;
  }
  return sum;
}

void set_null(S** pp) { *pp = nullptr; }

int range_for_init_expr_null_deref_bad() {
  S s{0};
  S* p = &s;
  int arr[3] = {1, 2, 3};
  int sum = 0;
  for (set_null(&p); int v : arr) {
    sum += v + p->x;
  }
  return sum;
}

struct Vec {
  int* data;
  int size;
  int* begin() { return data; }
  int* end() { return data + size; }
};

int range_for_over_init_variable_null_deref_bad() {
  int sum = 0;
  for (Vec* vec = nullptr; int v : *vec) {
    sum += v;
  }
  return sum;
}

int range_for_init_destructor_ok() {
  int arr[3] = {1, 2, 3};
  int sum = 0;
  for (Owner o; int v : arr) {
    sum += v + *o.p;
  }
  return sum;
}

int range_for_init_destructor_on_break_ok(int k) {
  int arr[3] = {1, 2, 3};
  int sum = 0;
  for (Owner o; int v : arr) {
    if (v == k) {
      break;
    }
    sum += v + *o.p;
  }
  return sum;
}

int range_for_init_destructor_on_return_ok(int k) {
  int arr[3] = {1, 2, 3};
  for (Owner o; int v : arr) {
    if (v == k) {
      return *o.p;
    }
  }
  return 0;
}

int range_for_init_lifetime_extended_temporary_ok() {
  int arr[3] = {1, 2, 3};
  int sum = 0;
  for (const Owner& o = Owner(); int v : arr) {
    sum += v + *o.p;
  }
  return sum;
}

struct OwnerRange {
  Owner& owner;
  int* begin() { return owner.p; }
  int* end() { return owner.p + 1; }
  ~OwnerRange() { *owner.p = 0; }
};

// the range temporary is destroyed before the init-statement variable it uses
int range_for_init_destroyed_after_range_ok() {
  int sum = 0;
  for (Owner o; int v : OwnerRange{o}) {
    sum += v;
  }
  return sum;
}

int range_for_init_use_after_destructor_bad() {
  int arr[3] = {1, 2, 3};
  int* q = nullptr;
  for (Owner o; int v : arr) {
    q = o.p;
  }
  return *q;
}

// goto does not destroy the variables of the scopes it exits, so the memory
// owned by [o] is reported as leaked
int FP_range_for_init_destructor_on_goto_ok(int k) {
  int arr[3] = {1, 2, 3};
  for (Owner o; int v : arr) {
    if (v == k) {
      goto out;
    }
  }
out:
  return 0;
}

int range_for_function_decl_init_bad() {
  int arr[3] = {1, 2, 3};
  for (int helper(int); int v : arr) {
    (void)v;
  }
  int* p = nullptr;
  return *p;
}

int switch_init_null_deref_bad(int k) {
  switch (S* p = nullptr; k) {
    case 0:
      return p->x;
    default:
      return 0;
  }
}

int switch_init_destructor_ok(int k) {
  switch (Owner o; k) {
    case 0:
      return *o.p;
    default:
      break;
  }
  return 0;
}

int switch_init_destructor_on_continue_ok(int n, int k) {
  int sum = 0;
  for (int i = 0; i < n; i++) {
    switch (Owner o; k) {
      case 0:
        continue;
      default:
        sum += *o.p;
        break;
    }
  }
  return sum;
}

// [o2] is destroyed by [continue] but [o1] is not
int range_for_init_switch_init_destructor_on_continue_ok() {
  int arr[3] = {1, 2, 3};
  int sum = 0;
  for (Owner o1; int v : arr) {
    switch (Owner o2; v) {
      case 1:
        continue;
      default:
        sum += *o1.p + *o2.p;
        break;
    }
  }
  return sum;
}

int switch_init_use_after_destructor_bad(int k) {
  int* q = nullptr;
  switch (Owner o; k) {
    case 0:
      q = o.p;
      break;
    default:
      q = o.p;
      break;
  }
  return *q;
}

int switch_init_constant_condition_null_deref_bad() {
  switch (S* p = nullptr; 0) {
    case 0:
      return p->x;
    default:
      return 0;
  }
}

int switch_init_default_only_null_deref_bad(int k) {
  S s{0};
  S* p = &s;
  switch (set_null(&p); k) {
    default:
      break;
  }
  return p->x;
}

int switch_init_empty_body_null_deref_bad(int k) {
  S s{0};
  S* p = &s;
  switch (set_null(&p); k) {}
  return p->x;
}

int switch_init_destructor_empty_body_ok(int k) {
  switch (Owner o; k) {}
  return 0;
}

int switch_enum_init_bad(int k) {
  switch (enum Color{Red, Green} c = k ? Red : Green; c) {
    default:
      break;
  }
  int* p = nullptr;
  return *p;
}

int switch_init_discarded_load_null_deref_bad(int k) {
  S* p = nullptr;
  switch ((void)p->x; k) {
    case 0:
      return 1;
    default:
      return 0;
  }
}

int if_init_discarded_load_null_deref_bad(int k) {
  S* p = nullptr;
  if ((void)p->x; k == 0) {
    return 1;
  }
  return 0;
}

int if_init_typedef_null_deref_bad(int k) {
  S* p = nullptr;
  if (typedef int T; k == 0) {
    return p->x;
  }
  return 0;
}

} // namespace init_statements
