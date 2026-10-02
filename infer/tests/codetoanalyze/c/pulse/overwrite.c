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
#include <strings.h>
#include <sys/stat.h>
#include <unistd.h>

void explicit_bzero(void* s, size_t n);

struct ops {
  int v;
};

struct dev {
  int id;
  struct ops* ops;
};

struct hdr {
  int type;
  int len;
};

struct msg {
  struct hdr h;
  struct ops* ops;
};

int bzero_null_field_bad() {
  struct dev d;
  bzero(&d, sizeof(struct dev));
  return d.ops->v;
}

int explicit_bzero_null_field_bad() {
  struct dev d;
  explicit_bzero(&d, sizeof(struct dev));
  return d.ops->v;
}

int builtin_memset_null_field_bad() {
  struct dev d;
  __builtin_memset(&d, 0, sizeof(struct dev));
  return d.ops->v;
}

int memset_memcpy_unknown_size_ok(const void* src, size_t n) {
  struct dev d;
  memset(&d, 0, sizeof(struct dev));
  memcpy(&d, src, n);
  return d.ops->v;
}

void copy_dev(struct dev* dst, const struct dev* src) {
  memmove(dst, src, sizeof(*dst));
}

int memset_memmove_in_callee_ok(const struct dev* src) {
  struct dev d;
  memset(&d, 0, sizeof(struct dev));
  copy_dev(&d, src);
  return d.ops->v;
}

int read_dev(int fd, struct dev* d) {
  return read(fd, d, sizeof(*d)) == sizeof(*d);
}

int memset_read_in_callee_ok(int fd) {
  struct dev d;
  memset(&d, 0, sizeof(struct dev));
  if (!read_dev(fd, &d)) {
    return -1;
  }
  return d.ops->v;
}

int fread_dev(FILE* f, struct dev* d) {
  return fread(d, 1, sizeof(*d), f) == sizeof(*d);
}

int memset_fread_in_callee_ok(FILE* f) {
  struct dev d;
  memset(&d, 0, sizeof(struct dev));
  if (!fread_dev(f, &d)) {
    return -1;
  }
  return d.ops->v;
}

void copy_bytes(void* dst, const void* src, size_t n) { memcpy(dst, src, n); }

int memset_copy_bytes_in_callee_ok(const struct dev* src) {
  struct dev d;
  memset(&d, 0, sizeof(struct dev));
  copy_bytes(&d, src, sizeof(struct dev));
  return d.ops->v;
}

void copy_bytes_in_callee_leak_bad(const struct dev* src) {
  struct dev* d = malloc(sizeof(struct dev));
  if (!d) {
    return;
  }
  copy_bytes(d, src, sizeof(struct dev));
}

int read_bytes(int fd, void* buf, size_t len) {
  char* p = buf;
  while (len > 0) {
    ssize_t n = read(fd, p, len);
    if (n <= 0) {
      return 0;
    }
    p += n;
    len -= n;
  }
  return 1;
}

int memset_read_bytes_in_callee_ok(int fd) {
  struct dev d;
  memset(&d, 0, sizeof(struct dev));
  if (!read_bytes(fd, &d, sizeof(struct dev))) {
    return -1;
  }
  return d.ops->v;
}

void memset_read_bytes_in_callee_then_check_bad(int fd) {
  struct dev d;
  memset(&d, 0, sizeof(struct dev));
  if (!read_bytes(fd, &d, sizeof(struct dev)) || d.id != 42) {
    return;
  }
  int* p = NULL;
  *p = 42;
}

int copy_bytes_then_read_ops(struct dev* d, const void* src, size_t n) {
  memcpy(d, src, n);
  return d->ops->v;
}

int memset_copy_bytes_then_read_in_callee_ok(const struct dev* src) {
  struct dev d;
  memset(&d, 0, sizeof(struct dev));
  return copy_bytes_then_read_ops(&d, src, sizeof(struct dev));
}

int memset_copy_header_null_field_bad(const struct hdr* h) {
  struct msg m;
  memset(&m, 0, sizeof(struct msg));
  memcpy(&m, h, sizeof(struct hdr));
  return m.ops->v;
}

void memset_read_header_bad(int fd) {
  struct msg m;
  memset(&m, 0, sizeof(struct msg));
  if (read(fd, &m, sizeof(struct hdr)) <= 0) {
    return;
  }
  if (m.h.type == 1) {
    int* p = NULL;
    *p = 42;
  }
}

ssize_t read_first_field_aliases_object_ok(int fd, struct msg* m) {
  if ((void*)&m->h != (void*)m) {
    return -1;
  }
  return read(fd, m, sizeof(struct msg));
}

int stat_file(const char* path, struct stat* st) { return stat(path, st); }

void memset_stat_in_callee_bad(const char* path) {
  struct stat st;
  memset(&st, 0, sizeof(struct stat));
  if (stat_file(path, &st) == 0 && st.st_size != 0) {
    int* p = NULL;
    *p = 42;
  }
}

int memset_init_through_function_pointer_ok(void (*init)(struct dev*)) {
  struct dev d;
  memset(&d, 0, sizeof(struct dev));
  init(&d);
  return d.ops->v;
}

struct dev_ops {
  void (*init)(struct dev*);
};

int memset_init_through_ops_table_ok(const struct dev_ops* dev_ops) {
  struct dev d;
  memset(&d, 0, sizeof(struct dev));
  dev_ops->init(&d);
  return d.ops->v;
}

void (*global_init)(struct dev*);

int memset_init_through_global_callback_ok() {
  struct dev d;
  memset(&d, 0, sizeof(struct dev));
  global_init(&d);
  return d.ops->v;
}

int memset_function_pointer_const_arg_bad(void (*use)(const struct dev*)) {
  struct dev d;
  memset(&d, 0, sizeof(struct dev));
  const struct dev* p = &d;
  use(p);
  return d.ops->v;
}

struct frame {
  char tag[8];
  struct ops* ops;
};

// copying an array that is the first field overwrites the whole object
int FN_memset_copy_first_array_field_null_field_bad(const char* tag) {
  struct frame f;
  memset(&f, 0, sizeof(struct frame));
  memcpy(&f, tag, sizeof(f.tag));
  return f.ops->v;
}

struct cells8 {
  long a, b, c, d, e, f, g, h;
};

struct big {
  struct ops* ops;
  struct cells8 c1, c2, c3, c4, c5, c6, c7, c8;
};

int memset_memcpy_big_struct_ok(const struct big* src) {
  struct big b;
  memset(&b, 0, sizeof(struct big));
  memcpy(&b, src, sizeof(struct big));
  return b.ops->v;
}

// objects with more than 64 scalar and pointer cells are not copied cell by
// cell
int FN_memcpy_big_struct_copies_null_bad(const struct big* src) {
  struct big b;
  memcpy(&b, src, sizeof(struct big));
  b.ops = NULL;
  struct big c;
  memcpy(&c, &b, sizeof(struct big));
  return c.ops->v;
}
