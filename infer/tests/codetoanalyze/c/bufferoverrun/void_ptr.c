/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <string.h>

void fill_void(void* dst, size_t n) { memset(dst, 0, n); }

void fill_void_char_Bad() {
  char buf[16];
  fill_void(buf, 17);
}

void fill_void_int_Good() {
  int buf[16];
  fill_void(buf, 64);
}

void fill_void_int_Bad() {
  int buf[16];
  fill_void(buf, 65);
}

void fill_void_int_offset_Good() {
  int buf[4];
  fill_void(buf + 2, 8);
}

void fill_void_int_offset_Bad() {
  int buf[4];
  fill_void(buf + 3, 8);
}

void fill_void_malloc_Bad() {
  int* p = (int*)malloc(4 * sizeof(int));
  if (p) {
    fill_void(p, 17);
    free(p);
  }
}

// The size of the array and the number of bytes written are both [4, 400], with
// no relation kept between them.
void FP_fill_void_vla_Good(size_t n) {
  if (n == 0 || n > 100) {
    return;
  }
  int buf[n];
  fill_void(buf, n * sizeof(int));
}

void fill_void_wrapper(void* p, size_t n) { fill_void(p, n); }

void fill_void_wrapper_Good() {
  int buf[4];
  fill_void_wrapper(buf, 16);
}

void fill_void_wrapper_Bad() {
  char buf[16];
  fill_void_wrapper(buf, 17);
}

void copy_void(void* dst, const void* src, size_t n) { memcpy(dst, src, n); }

void copy_void_Good() {
  int dst[4];
  char src[16] = {0};
  copy_void(dst, src, 16);
}

void copy_void_Bad() {
  char dst[8];
  char src[16] = {0};
  copy_void(dst, src, 16);
}

void set_byte(void* p, int i) { ((char*)p)[i] = 0; }

void set_byte_char_Bad() {
  char buf[16];
  set_byte(buf, 16);
}

void set_byte_int_Good() {
  int buf[16];
  set_byte(buf, 63);
}

void set_byte_int_Bad() {
  int buf[16];
  set_byte(buf, 64);
}

void set_byte_underrun_Bad() {
  int buf[16];
  set_byte(buf, -1);
}

void set_bytes(void* p, size_t n) {
  char* c = (char*)p;
  for (size_t i = 0; i < n; i++) {
    c[i] = 0;
  }
}

void set_bytes_Good() {
  int buf[4];
  set_bytes(buf, 16);
}

void set_bytes_Bad() {
  char buf[16];
  set_bytes(buf, 17);
}

void set_int(void* p, int i) { ((int*)p)[i] = 0; }

void set_int_Good() {
  int buf[16];
  set_int(buf, 15);
}

void set_int_Bad() {
  int buf[16];
  set_int(buf, 16);
}

void set_int_char_Good() {
  char buf[16];
  set_int(buf, 3);
}

void set_int_char_Bad() {
  char buf[16];
  set_int(buf, 4);
}

int* as_int(void* p) { return (int*)p; }

int as_int_Good() {
  int buf[16] = {0};
  return as_int(buf)[15];
}

int as_int_Bad() {
  int buf[16] = {0};
  return as_int(buf)[16];
}

void clear_ints(void* p, size_t n) {
  int* a = (int*)p;
  for (size_t i = 0; i < n / sizeof(int); i++) {
    a[i] = 0;
  }
}

// The division of a symbolic value is approximated by the value itself, so the
// loop seems to write n ints.
void FP_clear_ints_Good() {
  int buf[4];
  clear_ints(buf, sizeof(buf));
}

void* id_void(void* p) { return p; }

char id_void_char_Good() {
  int buf[16] = {0};
  char* c = (char*)id_void(buf);
  return c[63];
}

char id_void_char_Bad() {
  int buf[16] = {0};
  char* c = (char*)id_void(buf);
  return c[64];
}

struct pair {
  int a;
  int b;
};

void fill_pairs(struct pair* p, size_t n) {
  fill_void(p, n * sizeof(struct pair));
}

void fill_pairs_Good() {
  struct pair p[2];
  fill_pairs(p, 2);
}

void fill_pairs_Bad() {
  struct pair p[2];
  fill_pairs(p, 3);
}

void set_pair(void* p, int i) { ((struct pair*)p)[i].a = 0; }

void set_pair_Good() {
  struct pair p[2];
  set_pair(p, 1);
}

// Only casts to pointers to integers change the unit of the offset and size of
// a void* buffer, so the index below is compared with the size in bytes.
void FN_set_pair_Bad() {
  struct pair p[2];
  set_pair(p, 2);
}
