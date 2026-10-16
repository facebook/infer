/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

struct X {
  int f;
};

typedef struct X X;

void memcpy_ok() {
  X x;
  X* p = malloc(sizeof(X));
  if (p)
    memcpy(p, &x, sizeof(X));
  free(p);
}

void memcpy_to_null_bad() {
  X x;
  X* p = NULL;
  memcpy(p, &x, sizeof(X)); // crash
}

void memcpy_to_null_indirect_bad() {
  X x;
  X* r;
  X* p = NULL;
  r = p;
  memcpy(r, &x, sizeof(X)); // crash
}

void memcpy_from_null_bad() {
  X* src = NULL;
  X* p = malloc(sizeof(X));
  if (p) {
    memcpy(p, src, sizeof(X)); // crash
    free(p);
  }
}

struct Y {
  int* p;
};

int memset_memcpy_ok(const struct Y* src) {
  struct Y y;
  memset(&y, 0, sizeof(struct Y));
  memcpy(&y, src, sizeof(struct Y));
  return *y.p;
}

void memcpy_overwrites_pointer_leak_bad(const struct Y* src) {
  struct Y y;
  y.p = malloc(sizeof(int));
  memcpy(&y, src, sizeof(struct Y));
  free(y.p);
}

void swap_y(struct Y* a, struct Y* b) {
  struct Y tmp;
  memcpy(&tmp, a, sizeof(struct Y));
  memcpy(a, b, sizeof(struct Y));
  memcpy(b, &tmp, sizeof(struct Y));
}

void memcpy_swap_ok() {
  struct Y a, b;
  a.p = malloc(sizeof(int));
  b.p = malloc(sizeof(int));
  swap_y(&a, &b);
  free(a.p);
  free(b.p);
}

int memcpy_copies_null_bad() {
  struct Y src, dst;
  src.p = NULL;
  memcpy(&dst, &src, sizeof(struct Y));
  return *dst.p;
}

struct Z {
  struct X x;
  int* p;
};

void memcpy_first_field_keeps_pointer_ok(const struct X* src) {
  struct Z z;
  z.p = malloc(sizeof(int));
  memcpy(&z, src, sizeof(struct X));
  free(z.p);
}

void memcpy_first_field_aliases_object_ok(struct Z* z, const struct X* src) {
  if ((void*)&z->x == (void*)z) {
    memcpy(z, src, sizeof(struct X));
  }
}

struct W {
  struct Y y;
  int n;
};

void memcpy_first_field_aliases_object_keeps_pointer_ok(struct W* w) {
  if ((void*)&w->y != (void*)w) {
    return;
  }
  struct W tmp;
  tmp.y.p = malloc(sizeof(int));
  tmp.n = 0;
  memcpy(w, &tmp, sizeof(struct W));
}

struct msg {
  char tag[8];
  char* payload;
};

int memcpy_prefix_const_ok(const char* t) {
  struct msg* m = malloc(sizeof(struct msg));
  if (!m) {
    return 0;
  }
  m->payload = malloc(16);
  memcpy(m, t, 8);
  free(m->payload);
  free(m);
  return 0;
}

int memcpy_prefix_offsetof_ok(const char* t) {
  struct msg* m = malloc(sizeof(struct msg));
  if (!m) {
    return 0;
  }
  m->payload = malloc(16);
  memcpy(m, t, offsetof(struct msg, payload));
  free(m->payload);
  free(m);
  return 0;
}

int memcpy_prefix_sizeof_first_field_ok(const char* t) {
  struct msg* m = malloc(sizeof(struct msg));
  if (!m) {
    return 0;
  }
  m->payload = malloc(16);
  memcpy(m, t, sizeof(m->tag));
  free(m->payload);
  free(m);
  return 0;
}

void memcpy_prefix_stack_ok(const char* t) {
  struct msg m;
  m.payload = malloc(16);
  memcpy(&m, t, 8);
  free(m.payload);
}

void read_prefix_ok(int fd) {
  struct msg m;
  m.payload = malloc(16);
  if (read(fd, &m, 8) < 0) {
    free(m.payload);
    return;
  }
  free(m.payload);
}

void fread_prefix_ok(FILE* f) {
  struct msg m;
  m.payload = malloc(16);
  fread(&m, 1, 8, f);
  free(m.payload);
}

struct hdr {
  int type;
  int len;
};

struct packet {
  char* data;
  int len;
};

void memcpy_header_not_first_field_ok(const struct hdr* h) {
  struct packet p;
  p.data = malloc(16);
  memcpy(&p, h, sizeof(struct hdr));
  free(p.data);
}

void copy_prefix(void* dst, const void* src, size_t n) { memcpy(dst, src, n); }

void memcpy_prefix_in_callee_ok(const char* t) {
  struct msg m;
  m.payload = malloc(16);
  copy_prefix(&m, t, 8);
  free(m.payload);
}
