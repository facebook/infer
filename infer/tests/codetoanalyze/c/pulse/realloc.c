/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>
#include <stdlib.h>

int realloc_tmp_variable_ok() {
  int* p = (int*)malloc(sizeof(int));
  if (p == NULL) {
    return -1;
  }
  int* tmp = (int*)realloc(p, 2 * sizeof(int));
  if (tmp == NULL) {
    free(p); // a failed realloc leaves p allocated
    return -1;
  }
  p = tmp;
  p[1] = 42;
  free(p);
  return 0;
}

void realloc_failure_keep_original_ok() {
  int* p = (int*)malloc(sizeof(int));
  if (p == NULL) {
    return;
  }
  int* q = (int*)realloc(p, 2 * sizeof(int));
  if (q != NULL) {
    p = q;
  }
  *p = 42;
  free(p);
}

int* realloc_grow_in_loop_ok(int n) {
  int* buf = (int*)malloc(sizeof(int));
  if (buf == NULL) {
    return NULL;
  }
  for (int i = 1; i < n; i++) {
    int* tmp = (int*)realloc(buf, (i + 1) * sizeof(int));
    if (tmp == NULL) {
      free(buf);
      return NULL;
    }
    buf = tmp;
    buf[i] = i;
  }
  return buf;
}

void realloc_overwrites_original_leak_bad() {
  int* p = (int*)malloc(sizeof(int));
  if (p == NULL) {
    return;
  }
  p = (int*)realloc(p, 2 * sizeof(int)); // leaks the old block on failure
  free(p);
}

void realloc_failure_exit_ok() {
  int* p = (int*)malloc(sizeof(int));
  if (p == NULL) {
    exit(1);
  }
  p = (int*)realloc(p, 2 * sizeof(int));
  if (p == NULL) {
    exit(1);
  }
  free(p);
}

void realloc_failure_assert_ok() {
  int* p = (int*)malloc(sizeof(int));
  assert(p != NULL);
  p = (int*)realloc(p, 2 * sizeof(int));
  assert(p != NULL);
  free(p);
}

void use_original_after_realloc_bad() {
  int* p = (int*)malloc(sizeof(int));
  if (p == NULL) {
    return;
  }
  int* q = (int*)realloc(p, 2 * sizeof(int));
  if (q == NULL) {
    free(p);
    return;
  }
  *p = 42; // a successful realloc frees p
  free(q);
}

void free_original_after_realloc_bad() {
  int* p = (int*)malloc(sizeof(int));
  if (p == NULL) {
    return;
  }
  int* q = (int*)realloc(p, 2 * sizeof(int));
  if (q == NULL) {
    free(p);
    return;
  }
  free(q);
  free(p); // a successful realloc frees p
}

void realloc_after_free_bad() {
  int* p = (int*)malloc(sizeof(int));
  free(p);
  int* q = (int*)realloc(p, 2 * sizeof(int));
  free(q);
}

void realloc_of_null_leak_bad() { int* p = (int*)realloc(NULL, sizeof(int)); }

void realloc_of_null_free_ok() {
  int* p = NULL;
  int* q = (int*)realloc(p, sizeof(int));
  free(q);
}

// like BSD reallocf(): frees the original block if the reallocation fails
void* realloc_or_free_original(void* p, size_t size) {
  void* q = realloc(p, size);
  if (q == NULL) {
    free(p);
  }
  return q;
}

void realloc_or_free_original_ok() {
  int* p = (int*)malloc(sizeof(int));
  p = (int*)realloc_or_free_original(p, 2 * sizeof(int));
  free(p);
}

// leaves *buf unchanged if the reallocation fails
int grow_int_buffer(int** buf, size_t n) {
  int* tmp = (int*)realloc(*buf, n * sizeof(int));
  if (tmp == NULL) {
    return -1;
  }
  *buf = tmp;
  return 0;
}

void grow_int_buffer_ok() {
  int* buf = (int*)malloc(sizeof(int));
  grow_int_buffer(&buf, 2);
  free(buf);
}

void grow_int_buffer_failure_leak_bad() {
  int* buf = (int*)malloc(sizeof(int));
  if (grow_int_buffer(&buf, 2) != 0) {
    return;
  }
  free(buf);
}

void grow_int_buffer_use_original_bad() {
  int* buf = (int*)malloc(sizeof(int));
  if (buf == NULL) {
    return;
  }
  int* old = buf;
  if (grow_int_buffer(&buf, 2) == 0) {
    *old = 42;
  }
  free(buf);
}

// loses the original block if the reallocation fails
int grow_int_buffer_leaky(int** buf, size_t n) {
  *buf = (int*)realloc(*buf, n * sizeof(int));
  if (*buf == NULL) {
    return -1;
  }
  return 0;
}

void grow_int_buffer_leaky_caller_bad() {
  int* buf = (int*)malloc(sizeof(int));
  if (buf == NULL) {
    return;
  }
  grow_int_buffer_leaky(&buf, 2);
  free(buf);
}

// custom allocators, see .inferconfig
void* my_malloc(size_t size);
void my_free(void* p);
void* my_realloc(void* p, size_t size);

void custom_realloc_overwrites_original_leak_bad() {
  int* p = (int*)my_malloc(sizeof(int));
  if (p == NULL) {
    return;
  }
  p = (int*)my_realloc(p, 2 * sizeof(int));
  my_free(p);
}

void custom_realloc_free_original_bad() {
  int* p = (int*)my_malloc(sizeof(int));
  int* q = (int*)my_realloc(p, 2 * sizeof(int));
  if (q == NULL) {
    my_free(p);
    return;
  }
  my_free(q);
  my_free(p);
}

// g_realloc() never returns NULL for a non-zero size
void* g_malloc(size_t n_bytes);
void* g_realloc(void* mem, size_t n_bytes);
void g_free(void* mem);

void glib_realloc_overwrite_ok() {
  char* p = (char*)g_malloc(4);
  p = (char*)g_realloc(p, 8);
  *p = 'a';
  g_free(p);
}

void glib_realloc_use_original_bad() {
  char* p = (char*)g_malloc(4);
  char* q = (char*)g_realloc(p, 8);
  *p = 'a';
  g_free(q);
}

// glibc and scudo free p and return NULL for a zero size, so free(p) is a
// double free there; the model keeps p allocated whenever realloc returns NULL
void FN_realloc_zero_size_double_free_bad() {
  int* p = (int*)malloc(sizeof(int));
  if (p == NULL) {
    return;
  }
  int* q = (int*)realloc(p, 0);
  if (q == NULL) {
    free(p);
    return;
  }
  free(q);
}

// glibc and scudo free p in realloc(p, 0), but the model keeps p allocated
// when realloc returns NULL, so the old block is reported as leaked
void FP_realloc_zero_size_ok() {
  int* p = (int*)malloc(sizeof(int));
  if (p == NULL) {
    return;
  }
  p = (int*)realloc(p, 0);
  free(p);
}

void realloc_fatal_error(void) __attribute__((noreturn));

// Pulse checks for leaks when a path ends in a call to a noreturn function that
// is not modelled like exit() or abort(), and a failed realloc leaves p
// allocated
void FP_realloc_failure_noreturn_ok() {
  int* p = (int*)malloc(sizeof(int));
  if (p == NULL) {
    return;
  }
  int* q = (int*)realloc(p, 2 * sizeof(int));
  if (q == NULL) {
    realloc_fatal_error();
  }
  free(q);
}
