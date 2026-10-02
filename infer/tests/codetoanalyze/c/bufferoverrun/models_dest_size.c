/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <assert.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

size_t strlcpy(char* dst, const char* src, size_t size);
size_t strlcat(char* dst, const char* src, size_t size);

void snprintf_Good(int v) {
  char buf[16];
  snprintf(buf, sizeof(buf), "%d", v);
}

void snprintf_Bad(int v) {
  char buf[16];
  snprintf(buf, 17, "%d", v);
}

void snprintf_var_Bad(int v, int big) {
  char buf[16];
  size_t n = big ? 64 : 8;
  snprintf(buf, n, "%d", v);
}

int snprintf_size_query_Good(int v) { return snprintf(NULL, 0, "%d", v); }

void snprintf_offset_Bad(int v) {
  char buf[16];
  snprintf(buf + 8, 9, "%d", v);
}

void snprintf_index_Bad(int a) {
  char buf[64];
  snprintf(&buf[8], sizeof(buf), "%d", a);
}

void snprintf_size_param(size_t n, int v) {
  char buf[16];
  snprintf(buf, n, "%d", v);
}

void call_snprintf_size_param_Good() { snprintf_size_param(16, 0); }

void call_snprintf_size_param_Bad() { snprintf_size_param(32, 0); }

void vsnprintf_Good(const char* fmt, va_list args) {
  char buf[16];
  vsnprintf(buf, sizeof(buf), fmt, args);
}

void vsnprintf_Bad(const char* fmt, va_list args) {
  char buf[16];
  vsnprintf(buf, 32, fmt, args);
}

void strlcpy_Good(const char* s) {
  char buf[16];
  strlcpy(buf, s, sizeof(buf));
}

void strlcpy_Bad(const char* s) {
  char buf[16];
  strlcpy(buf, s, 32);
}

void strlcat_Good(const char* s) {
  char buf[16] = "";
  strlcat(buf, s, sizeof(buf));
}

void strlcat_Bad(const char* s) {
  char buf[16] = "";
  strlcat(buf, s, 32);
}

void strncat_Good(const char* s) {
  char buf[8];
  strcpy(buf, "abc");
  strncat(buf, s, 4);
}

void strncat_Bad() {
  char buf[8];
  strcpy(buf, "abc");
  strncat(buf, "defgh", 5);
}

void strncat_short_src_Good() {
  char buf[8];
  strcpy(buf, "abc");
  strncat(buf, "de", 100);
}

void strncat_param(const char* s) {
  char buf[8];
  strcpy(buf, "abc");
  strncat(buf, s, 5);
}

void call_strncat_param_Good() { strncat_param("defg"); }

void call_strncat_param_Bad() { strncat_param("defgh"); }

void strncat_strlen_Bad() {
  char buf[16];
  strcpy(buf, "abc");
  strncat(buf, "defgh", 3);
  int a[6];
  a[strlen(buf)] = 0;
}

void strncat_remaining_size_Good(int short_prefix) {
  char buf[16];
  if (short_prefix) {
    strcpy(buf, "abc");
  } else {
    strcpy(buf, "abcdefgh");
  }
  strncat(buf, "0123456789abcdef", sizeof(buf) - strlen(buf) - 1);
}

void strncat_after_fgets_Good(FILE* f) {
  char buf[64];
  if (fgets(buf, sizeof(buf), f) == NULL) {
    return;
  }
  strncat(buf, "suffix", sizeof(buf) - strlen(buf) - 1);
}

void strncat_after_fgets_Bad(FILE* f) {
  char buf[64];
  if (fgets(buf, sizeof(buf), f) == NULL) {
    return;
  }
  strncat(buf, "suffix", sizeof(buf) - strlen(buf));
}

void read_Good(int fd) {
  char buf[16];
  read(fd, buf, sizeof(buf));
}

void read_Bad(int fd) {
  char buf[16];
  read(fd, buf, 32);
}

void read_nul_terminate_Good(int fd) {
  char buf[16];
  ssize_t n = read(fd, buf, sizeof(buf) - 1);
  if (n < 0) {
    return;
  }
  buf[n] = '\0';
}

void read_nul_terminate_Bad(int fd) {
  char buf[16];
  ssize_t n = read(fd, buf, sizeof(buf));
  if (n < 0) {
    return;
  }
  buf[n] = '\0';
}

void read_error_not_checked_Bad(int fd) {
  char buf[16];
  ssize_t n = read(fd, buf, sizeof(buf) - 1);
  buf[n] = '\0';
}

void pread_Bad(int fd) {
  char buf[16];
  pread(fd, buf, 32, 0);
}

void readlink_nul_terminate_Good(const char* path) {
  char buf[16];
  ssize_t n = readlink(path, buf, sizeof(buf) - 1);
  if (n < 0) {
    return;
  }
  buf[n] = '\0';
}

void readlink_nul_terminate_Bad(const char* path) {
  char buf[16];
  ssize_t n = readlink(path, buf, sizeof(buf));
  if (n < 0) {
    return;
  }
  buf[n] = '\0';
}

void recv_Good(int fd) {
  char buf[16];
  recv(fd, buf, sizeof(buf), 0);
}

void recv_Bad(int fd) {
  char buf[16];
  recv(fd, buf, 32, 0);
}

void recvfrom_Bad(int fd) {
  char buf[16];
  recvfrom(fd, buf, 32, 0, NULL, NULL);
}

void fread_Good(FILE* f) {
  int buf[4];
  fread(buf, sizeof(int), 4, f);
}

void fread_Bad(FILE* f) {
  int buf[4];
  fread(buf, sizeof(int), 5, f);
}

void fread_result_Bad(FILE* f) {
  int buf[4];
  size_t n = fread(buf, sizeof(int), 4, f);
  buf[n] = 0;
}

void recv_strncpy_Bad(int fd) {
  char src[40] = {0};
  recv(fd, src, sizeof(src), 0);
  char dst[100];
  strncpy(dst, src, 44);
}

void fread_strncpy_Bad(FILE* f) {
  char src[40] = {0};
  fread(src, 1, sizeof(src), f);
  char dst[100];
  strncpy(dst, src, 44);
}

void snprintf_strncpy_Good(int v) {
  char src[40];
  snprintf(src, sizeof(src), "%d", v);
  char dst[100];
  strncpy(dst, src, 44);
}

void strlcpy_strncpy_Good(const char* s) {
  char src[40];
  strlcpy(src, s, sizeof(src));
  char dst[100];
  strncpy(dst, src, 44);
}

void snprintf_strncpy_Bad(int v) {
  char src[40];
  snprintf(src, sizeof(src), "%d", v);
  char dst[30];
  strncpy(dst, src, 44);
}

void snprintf_append_Good(int a, int b) {
  char buf[64];
  int len = snprintf(buf, sizeof(buf), "%d", a);
  if (len >= sizeof(buf)) {
    return;
  }
  snprintf(buf + len, sizeof(buf) - len, "%d", b);
}

void snprintf_append_Bad(int a, int b) {
  char buf[64];
  int len = snprintf(buf, sizeof(buf), "%d", a);
  if (len >= sizeof(buf)) {
    return;
  }
  snprintf(buf + len, sizeof(buf) - len + 1, "%d", b);
}

void snprintf_append_offset_Bad(int a, int b) {
  char buf[64];
  int len = snprintf(buf, sizeof(buf), "%d", a);
  if (len > 100) {
    return;
  }
  snprintf(buf + len, sizeof(buf) - len, "%d", b);
}

void snprintf_append_index_Good(int a, int b) {
  char buf[64];
  int len = snprintf(buf, sizeof(buf), "%d", a);
  if (len >= sizeof(buf)) {
    return;
  }
  snprintf(&buf[len], sizeof(buf) - len, "%d", b);
}

void snprintf_append_index_Bad(int a, int b) {
  char buf[64];
  int len = snprintf(buf, sizeof(buf), "%d", a);
  if (len >= sizeof(buf)) {
    return;
  }
  snprintf(&buf[len], sizeof(buf) - len + 1, "%d", b);
}

void snprintf_end_pointer_Good(int a, int b) {
  char buf[64];
  char* p = buf;
  char* end = buf + sizeof(buf);
  int r = snprintf(p, end - p, "%d", a);
  if (r >= end - p) {
    return;
  }
  p += r;
  snprintf(p, end - p, "%d", b);
}

void snprintf_pointer_diff_Good(int a, int b) {
  char buf[64];
  char* p = buf;
  int r = snprintf(p, sizeof(buf), "%d", a);
  if (r >= sizeof(buf)) {
    return;
  }
  p += r;
  snprintf(p, sizeof(buf) - (p - buf), "%d", b);
}

void snprintf_end_index_Good(int a, int b) {
  char buf[64];
  char* p = buf;
  int r = snprintf(p, sizeof(buf), "%d", a);
  if (r >= sizeof(buf)) {
    return;
  }
  p += r;
  snprintf(p, &buf[sizeof(buf)] - p, "%d", b);
}

/* The interval domain cannot bound [r] by the size argument, which varies in
   the loop, so [total] is unbounded after the loop. */
void FP_read_loop_Good(int fd) {
  char buf[64];
  size_t total = 0;
  while (total < sizeof(buf) - 1) {
    ssize_t r = read(fd, buf + total, sizeof(buf) - 1 - total);
    if (r <= 0) {
      break;
    }
    total += r;
  }
  buf[total] = '\0';
}

static int read_fully(int fd, char* buf, size_t len) {
  while (len > 0) {
    ssize_t r = read(fd, buf, len);
    if (r <= 0) {
      return -1;
    }
    buf += r;
    len -= r;
  }
  return 0;
}

/* [len] is unbounded after [len -= r], as in FP_read_loop_Good. */
void FP_call_read_fully_Good(int fd) {
  char buf[64];
  read_fully(fd, buf, sizeof(buf));
}

/* [left] is unbounded after [left -= n], as in FP_read_loop_Good. */
void FP_fread_loop_Good(FILE* f) {
  char buf[64];
  char* p = buf;
  size_t left = sizeof(buf);
  while (left > 0) {
    size_t n = fread(p, 1, left, f);
    if (n == 0) {
      return;
    }
    p += n;
    left -= n;
  }
}

void fread_append_Good(FILE* f, size_t off) {
  char buf[64];
  if (off > sizeof(buf)) {
    return;
  }
  fread(buf + off, 1, sizeof(buf) - off, f);
}

void memset_append_Good() {
  char buf[64];
  if (fgets(buf, sizeof(buf), stdin) == NULL) {
    return;
  }
  size_t len = strlen(buf);
  memset(buf + len, 0, sizeof(buf) - len);
}

void snprintf_append_strlen_Good(int a) {
  char buf[64];
  if (fgets(buf, sizeof(buf), stdin) == NULL) {
    return;
  }
  snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf), "%d", a);
}

void snprintf_append_strlen_Bad(int a) {
  char buf[64];
  if (fgets(buf, sizeof(buf), stdin) == NULL) {
    return;
  }
  snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf) + 1, "%d", a);
}

void snprintf_append_strlen_offset_Bad(int a) {
  char buf[64];
  if (fgets(buf, sizeof(buf), stdin) == NULL) {
    return;
  }
  snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf + 1), "%d", a);
}

/* [buf + n] is [4 * n] bytes into [buf], so the two [n] do not cancel. */
void read_int_array_offset_Bad(int fd, int big) {
  int buf[16];
  int n = big ? 8 : 0;
  read(fd, buf + n, sizeof(buf) - n);
}

/* The interval domain does not relate [r] to [sizeof(buf) - off], so [off] may
   exceed the buffer size after [off += r]. */
void FP_snprintf_append_loop_Good(int* arr, int n) {
  char buf[64];
  size_t off = 0;
  for (int i = 0; i < n; i++) {
    int r = snprintf(buf + off, sizeof(buf) - off, "%d,", arr[i]);
    if (r >= sizeof(buf) - off) {
      break;
    }
    off += r;
  }
}

/* The interval domain does not relate [left] to [len]. */
void FP_snprintf_remaining_size_Good(int a, int b) {
  char buf[64];
  int len = snprintf(buf, sizeof(buf), "%d", a);
  if (len >= sizeof(buf)) {
    return;
  }
  size_t left = sizeof(buf) - len;
  snprintf(buf + len, left, "%d", b);
}

/* The interval domain does not relate the allocated size to [n]. */
void FP_read_malloc_copy_Good(int fd) {
  char buf[64];
  ssize_t n = read(fd, buf, sizeof(buf));
  if (n <= 0) {
    return;
  }
  char* p = malloc(n);
  if (p == NULL) {
    return;
  }
  memcpy(p, buf, n);
  free(p);
}

static void snprintf_dest_param(char* out, size_t size) {
  snprintf(out, size, "%d", 0);
}

void call_snprintf_dest_param_Good() {
  char buf[16];
  snprintf_dest_param(buf, sizeof(buf));
}

void call_snprintf_dest_param_Bad() {
  char buf[16];
  snprintf_dest_param(buf, 32);
}

void read_int_array_const_offset_Bad(int fd) {
  int buf[4];
  read(fd, buf + 2, 9);
}

static void read_into_u16(int fd, uint16_t* p, size_t n) { read(fd, p, n); }

void call_read_into_u16_Good(int fd) {
  uint16_t buf[4];
  read_into_u16(fd, buf, sizeof(buf));
}

void call_read_into_u16_Bad(int fd) {
  uint16_t buf[4];
  read_into_u16(fd, buf, 9);
}

/* Only a single value of the size bounds the result of [read], see
   FP_read_loop_Good. */
void FP_read_range_Good(int fd, int c) {
  char buf[8];
  int count = c ? 4 : 8;
  ssize_t n = read(fd, buf, count);
  if (n > 0) {
    buf[n - 1] = '\0';
  }
}

static char* split_string(const char* src, size_t left, size_t right) {
  assert(left <= right);
  char* dest = malloc(right - left + 1);
  if (dest == NULL) {
    return NULL;
  }
  memcpy(dest, src + left, right - left);
  dest[right - left] = '\0';
  return dest;
}

/* The interval domain does not keep [left <= right], so [right - left] may be
   negative in [split_string]. */
void FP_recv_split_string_Good(int fd) {
  char buf[64];
  ssize_t n = recv(fd, buf, sizeof(buf), 0);
  if (n <= 0) {
    return;
  }
  size_t left = 0;
  while (left < n && buf[left] == ' ') {
    left++;
  }
  free(split_string(buf, left, n));
}

char* get_string(void);

static int can_reuse(const char* target, size_t length) {
  size_t target_length = strlen(target);
  return target_length >= length &&
         (target_length < 32 || target_length - length < target_length / 2);
}

/* The interval domain does not keep [target_length >= length], so
   [target_length - length] may underflow once [length] is bounded. */
int FP_snprintf_guarded_subtraction_Good(double v) {
  char buf[128];
  snprintf(buf, sizeof(buf), "%g", v);
  size_t length = strlen(buf);
  return length > 0 && can_reuse(get_string(), length);
}

static char* prefix_message(const char* msg) {
  size_t len = strlen(msg);
  char* result = malloc(len + 5);
  if (result == NULL) {
    return NULL;
  }
  memcpy(result + 5, msg, len);
  return result;
}

/* The caller substitutes [strlen(msg)] by an interval in the offset and the
   size of [prefix_message] independently. */
void FP_snprintf_message_copy_Good(unsigned t) {
  char buf[40];
  snprintf(buf, sizeof(buf), "unknown type %u", t);
  free(prefix_message(buf));
}

static void append(char** buf, size_t* size, const char* format, ...) {
  va_list ap;
  va_start(ap, format);
  size_t n = vsnprintf(*buf, *size, format, ap);
  va_end(ap);
  if (n <= *size) {
    *size -= n;
    *buf += n;
  }
}

/* The interval domain does not keep [buf + size == buffer + 64] across the
   calls to [append]. */
void FP_vsnprintf_advance_Good(int a) {
  char buffer[64];
  char* buf = buffer;
  size_t size = sizeof(buffer);
  append(&buf, &size, "%d", a);
  append(&buf, &size, "\n");
}

struct entry {
  char name[8];
  int id;
};

/* The array fields of a local struct have no array value. */
void FN_strlcpy_local_struct_field_Bad(const char* s) {
  struct entry n;
  strlcpy(n.name, s, sizeof(n));
}

/* The array fields of a local struct have no array value. */
void FN_read_local_struct_field_Bad(int fd) {
  struct entry n;
  read(fd, n.name, sizeof(n.name) + 1);
}

/* Only an array of bytes is checked at [&a[i]]. */
void FN_read_struct_array_Bad(int fd) {
  struct entry a[4];
  read(fd, &a[1], sizeof(a));
}

void strlcpy_struct_field_param_Bad(struct entry* n, const char* s) {
  strlcpy(n->name, s, sizeof(*n));
}
