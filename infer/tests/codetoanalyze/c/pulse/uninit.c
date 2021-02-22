/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int dereference_bad() {
  int* p;
  return *p;
}

void self_assign_bad() {
  int x;
  x = x;
}

void use_and_mayinit(int, int*);

void call_to_use_and_mayinit_bad() {
  int x;
  use_and_mayinit(x, &x);
}

void malloc_good() {
  int* p = (int*)malloc(sizeof(int));
  if (p) {
    *p = 5;
    int x = *p;
  }
  free(p);
}

void malloc_bad() {
  int* p = (int*)malloc(sizeof(int));
  if (p) {
    int x = *p;
  }
  free(p);
}

void init_int_ref(int* p) { *p = 5; }

void interprocedural_init_in_callee_good() {
  int x;
  init_int_ref(&x);
  int y = x;
}

void nop(int* p) {}

void interprocedural_nop_in_callee_bad() {
  int x;
  nop(&x);
  int y = x;
}

void read_int_ref(int* p) { int x = *p; }

void interprocedural_read_in_callee_bad() {
  int x;
  read_int_ref(&x);
}

int* uninit() { return (int*)malloc(sizeof(int)); }

void interprocedural_uninit_in_callee_bad() {
  int* p = uninit();
  if (p) {
    int x = *p;
  }
}

struct uninit_s {
  int f1;
  int f2;
};

void get_field_address_good() {
  struct uninit_s* s = (struct uninit_s*)malloc(2 * sizeof(int));
  if (s) {
    int* p = &s->f1;
  }
  free(s);
}

void init_f1(struct uninit_s* p) { p->f1 = 5; }

void interprocedural_struct_good() {
  struct uninit_s s;
  init_f1(&s);
  int y = s.f1;
}

void interprocedural_struct_bad() {
  struct uninit_s s;
  init_f1(&s);
  int y = s.f2;
}

void malloc_array_good(int len) {
  char* o = (char*)malloc(len);
  if (o) {
    o[0] = 'a';
    char c = o[0];
  }
  free(o);
}

struct uninit_s unknown_struct();

struct uninit_s unknown_wrapper() {
  return unknown_struct();
}

void havoc_calling_unknown_struct_good() {
  struct uninit_s x = unknown_wrapper();
  int y = x.f1;
}

void malloc_array_bad_FN(int len) {
  char* o = (char*)malloc(len);
  if (o) {
    o[0] = 'a';
    char c = o[1];
  }
  free(o);
}

void local_array_good() {
  char o[10];
  o[0] = 'a';
  char c = o[0];
  free(o);
}

void local_array_bad_FN() {
  char o[10];
  o[0] = 'a';
  char c = o[1];
  free(o);
}

struct uninit_nested {
  struct uninit_s g1;
  int g2;
};

void read_g1_f1(struct uninit_nested* x) { int y = x->g1.f1; }

void nested_struct_good() {
  struct uninit_nested x;
  x.g1.f1 = 42;
  read_g1_f1(&x);
}

void nested_struct_bad() {
  struct uninit_nested x;
  x.g1.f2 = 42;
  read_g1_f1(&x);
}
