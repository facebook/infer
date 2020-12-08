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

void nop(int* p) {}

void interprocedural_good() {
  int x;
  init_int_ref(&x);
  int y = x;
}

void interprocedural_bad_FN() {
  int x;
  nop(&x);
  int y = x;
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

void interprocedural_struct_bad_FN() {
  struct uninit_s s;
  init_f1(&s);
  int y = s.f2;
}

void malloc_array_good(int len) {
  char* o = (char*)malloc(len);
  if (o) {
    o[0] = 'a';
  }
  free(o);
}
