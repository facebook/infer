/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <algorithm>

void init(int* i) { *i = 10; }

void init_bool(bool* i) { *i = false; }

void no_init(int* i) {}

void no_init_bool(bool* i) {}

int inc(int x) { return x + 1; }
// error is detected before call as we copy x
// so no need to put it in the summary

int no_init_return_bad() {
  int x;
  return x; // error
}

int bad1() {
  int a;
  int b = a; // Error
  int c = b; // Error but we do not report as it depends from line 20
  return c;
}

int ok3() {
  int a;
  int c;

  init(&a);
  c = a; // no report since the variable could be initialized when passed by
         // reference in previous call

  return c;
}

int ok4() {
  int a;
  int c;

  init(&a);

  c = inc(a); // no report since the variable could be initialized when passed
              // by reference in previous call
  return c;
}

void square_init(int x, int& res) { res = x * x; }

int square_no_init(int x, int& res) { return res * res; }

void use_square_ok1() {

  int i;
  square_init(2, i); // OK since i is initialized when passed by reference
}

int use_square_ok2() {

  int i;
  i = square_no_init(2, i); // OK only for intraprocedural case. When analysis
                            // extended to interprocedural, it should report.
  return i;
}

bool getOK(void);

int branch1_FP() {

  int size;

  bool ok = getOK();

  if (ok) {
    size = 1;
  }

  if (ok) {
    return size; // report here because size initialized only on the then-branch
                 // above
  }

  return 0;
}

int loop1_FP() {

  int size;

  for (;;) {
    size = 1;
    if (getOK())
      break;
  }

  return size; // report here because size initialized only inside loop
}

int ok6() {
  int x;
  x = 7;
  return x;
}

// this crashes HIL if we're not careful
void deref_magic_addr_ok() { *(int*)0xdeadbeef = 0; }

char ok7() {
  char buf[1024], *res = buf; // OK, because we copy an address
  res[1] = 'a';
  return res[1];
}

void use_an_int(int);

void bad2() {
  int a;
  use_an_int(a); // Report as we pass an unitialized value
}

void ok8() {
  int a;
  init(&a); // no report since the variable could be initialized when passed by
            // reference.
}

struct A {
  int* ptr;
  int value;
};

void ok9() {
  int i;
  A a;
  a.ptr = &i; // no report since the variable could be initialized when passed
              // by reference.
  init(a.ptr);
}

int field_passed_by_ref_ok() {
  A a;
  init(&a.value);
  return a.value;
}

void init_double_pointer(int**);

int pointer_passed_by_ref_ok() {
  int* i;
  init_double_pointer(&i);
  return *i;
}

int array_initialized_ok(int N, int index) {
  int array[N];
  std::fill_n(array, N, 0.0f);
  int value = array[index];
  return value;
}

int array_element_passed_by_ref_ok() {
  int array[1];
  init(&(array[0]));
  int value = array[0];
  return value;
}

int ret_undef_bad() {
  int* p;
  return *p; // report as p was not initialized
}

int copy_pointer_bad() {
  int* p;
  int* q;
  p = q; // error
  return *p;
}

void use_an_int2(int*);

int ok10() {
  int buf[1024];
  use_an_int2(buf); // no report as we pass the pointer to buf
  return 1;
}

void capture_read_bad() {
  int x;
  [x]() {
    int y = x;
    return;
  }(); // We should report that captured x is not init
}

void init_capture_read_ok() {
  int x;
  [x = 0]() {
    int y = x;
    return;
  }();
}

void init_capture_ok() {
  [i = 0]() { return i; };
}

void FN_capture_by_ref_reuseBad() {
  int x;
  [&x]() {
    int y = x;
  }(); // We don't report here as we only do intraprocedural analysis for now
}

int capture_by_ref_init_FP() {
  int x;
  [&x]() { x = 1; }();
  return x;
}

int capture_by_ref_init2_FP() {
  int x;
  auto lambda = [&x]() { x = 1; };
  lambda();
  return x;
}

int no_warning_on_throw_ok(bool t) {
  int x;
  if (t) {
    x = 2;
  } else {
    throw;
  }
  return x;
}

int warning_when_throw_in_other_branch_bad(int t) {
  int x;
  if (t > 0) {
    x = 2;
  } else if (t < 0) {
    // reports because x is not initialized in this branch
  } else {
    throw;
  }
  return x;
}

[[noreturn]] void noreturn_function() {}

int FP_no_warning_noreturn_callee_ok(bool t) {
  int x;
  if (t) {
    x = 2;
  } else {
    noreturn_function();
  }
  return x;
}

void some_f(void* p);

int* FP_pointer_param_void_star_ok() {
  A a;
  int* res;
  some_f(&a); // the type of a here is void*, hence no fields are found
  return a.ptr; // false positive
}

short union_ok() {
  union {
    int* a;
    short* b;
  } u;
  init(u.a);
  short* p = u.b;
  return *p;
}

int condition_no_init_bad() {
  int x;
  if (x) {
    return 1;
  }
  return 0;
}

void call_to_fn_ptr_with_init_arg_good(void (*f)(int)) {
  int a = 42;
  f(a);
}

void FN_call_to_fn_ptr_with_uninit_arg_bad(void (*f)(int)) {
  int a;
  f(a);
}

void call_to_init_fn_ptr_good() {
  void (*f)();
  f = use_square_ok1;
  f();
}

void call_to_init_fn_ptr2_good(bool nondet) {
  void (*f)();
  if (nondet)
    f = use_square_ok1;
  else
    f = deref_magic_addr_ok;
  f();
}

void FN_call_to_uninit_fn_ptr_bad() {
  void (*f)();
  f();
}

void FN_call_to_maybe_uninit_fn_ptr_bad(bool nondet) {
  void (*f)();
  if (nondet)
    f = use_square_ok1;
  f();
}

void use_uninit_in_expr_bad() {
  int x;
  int y = x + 2;
}
