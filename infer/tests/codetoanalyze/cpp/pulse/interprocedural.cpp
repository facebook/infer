/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct X {
  int f;
};

void skip(struct X& x) {}
void skip_ptr(struct X* x) {}

int wraps_read_inner(struct X& x) { return x.f; }

int wraps_read(struct X& x) { return wraps_read_inner(x); }

void wraps_write_inner(struct X& x, int i) { x.f = i; }

void wraps_write(struct X& x, int i) { wraps_write_inner(x, i); }

void wraps_delete_inner(struct X* x) { delete x; }

void wraps_delete(struct X* x) { wraps_delete_inner(x); }

void delete_then_skip_ok(struct X& x) {
  delete (&x);
  skip(x);
}

void delete_then_skip_ptr_ok(struct X* x) {
  delete x;
  skip_ptr(x);
}

void delete_then_read_bad(struct X& x) {
  delete (&x);
  wraps_read(x);
}

void delete_aliased_then_read_bad(struct X& x) {
  struct X* y = &x;
  struct X& z = x;
  delete y;
  wraps_read(z);
}

void delete_then_write_bad(struct X& x) {
  wraps_delete(&x);
  wraps_read(x);
}

void delete_inner_then_write_bad(struct X& x) {
  wraps_delete_inner(&x);
  wraps_read(x);
}

// latent because delete(&x) creates a path where &x==0 but it was dereferenced
// before, but that does not make sense as &x cannot be null
// Latent FP filtered out
void latent_read_write_then_delete_ok(struct X& x) {
  wraps_write(x, 10);
  wraps_read(x);
  wraps_delete(&x);
}

int two_cells(struct X* x, struct X* y) {
  x->f = 32;
  y->f = 52;
  return x->f * y->f;
}

void aliasing_call(struct X* x) { two_cells(x, x); }

struct Y {
  int* p;
};

void store(struct Y* y, int* p) { y->p = p; }

void call_store(struct Y* y) {
  int x = 42;
  store(y, &x);
}

extern bool nondet_choice();

struct Y* may_return_invalid_ptr_ok() {
  struct Y* y = new Y();
  if (nondet_choice()) {
    delete y;
  }
  return y;
}

void feed_invalid_into_access_bad() {
  struct Y* y = may_return_invalid_ptr_ok();
  call_store(y);
  delete y;
}

void invalidate_and_set_to_null(struct X** x_ptr) {
  delete (*x_ptr);
  *x_ptr = nullptr;
}

void access_to_invalidated_alias_bad(struct X* x, struct X* y) {
  y = x;
  invalidate_and_set_to_null(&x);
  wraps_read(*y);
}

void access_to_invalidated_alias2_bad(struct X* x, struct X* y) {
  y = x;
  invalidate_and_set_to_null(&y);
  wraps_read(*x);
}

void set_first_non_null_ok(int* x, int* y) {
  if (x) {
    *x = 42;
  } else {
    *y = 42;
  }
}

void set_x_then_crash_bad(int* x) {
  set_first_non_null_ok(x, nullptr);
  set_first_non_null_ok(nullptr, x);
  int* p = nullptr;
  *p = 42;
}
