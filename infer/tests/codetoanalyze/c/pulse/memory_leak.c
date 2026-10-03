/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

void malloc_no_free_bad() { int* p = malloc(sizeof(p)); }

int* malloc_returned_ok() {
  int* p = malloc(sizeof(p));
  return p;
}

void malloc_out_parameter_ok(int** x) { *x = (int*)malloc(sizeof(int)); }

void malloc_out_parameter_local_mutation_ok(int** x) {
  *x = (int*)malloc(sizeof(int));
  x = NULL; // not visible from the outside
}

void malloc_out_parameter_local_mutation_bad(int** x) {
  *x = (int*)malloc(sizeof(int));
  *x = NULL;
}

void malloc_then_free_ok() {
  int* p = malloc(sizeof(p));
  if (p) {
    *p = 5;
    free(p);
  }
}

int* create_p() {
  int* p = malloc(sizeof(p));
  return p;
}

void malloc_interproc_no_free_bad() { int* p = create_p(); }

void malloc_interproc_no_free_bad2() {
  int* p = malloc(sizeof(p));
  int z = 3;
  int y = 4;
  int* q = p;
}

void malloc_formal_leak_bad(int* x) { x = (int*)malloc(sizeof(int*)); }

static void* (*const malloc_func)(size_t) = malloc;
static void (*const free_func)(void*) = free;

void* malloc_via_ptr(size_t size) {
  void* ret = NULL;

  if (size <= 0) {
    return NULL;
  }

  ret = malloc_func(size);
  return ret;
}

void free_via_ptr(void* x) { free_func(x); }

void malloc_ptr_leak_bad() { int* p = (int*)malloc_via_ptr(sizeof(int)); }

void malloc_ptr_no_check_leak_bad() {
  int* p = (int*)malloc_via_ptr(sizeof(int));
  *p = 42;
}

void malloc_ptr_free_ok() {
  int* p = (int*)malloc_via_ptr(sizeof(int));
  free(p);
}

void malloc_ptr_free_ptr_ok() {
  int* p = (int*)malloc_via_ptr(sizeof(int));
  free_via_ptr(p);
}

void alias_ptr_free_ok(int* out, int flag) {
  int* y;
  if (flag) {
    y = (int*)malloc(sizeof(int));
  } else {
    y = out;
  }
  if (y && y != out) {
    free(y);
  }
}

void report_leak_in_correct_line_bad(int* x) {
  x = (int*)malloc(sizeof(int));
  if (x != NULL) {
    return; // should report leak at this line
  }
  free(x);
}

void* realloc_wrapper(void* p, size_t size) { return realloc(p, size); }

void realloc_free_ok() {
  int* p = (int*)malloc(sizeof(int));
  int* q = realloc_wrapper(p, sizeof(int));
  if (!q) {
    free(p);
    return;
  }
  free(q);
}

void realloc_no_free_bad() {
  int* p = (int*)malloc(sizeof(int));
  int* q = realloc_wrapper(p, sizeof(int));
}

void realloc_no_check_bad() {
  int* p = (int*)malloc(sizeof(int));
  int* q = realloc_wrapper(p, sizeof(int));
  *q = 42;
  free(q);
}

void* my_malloc(size_t size);
void* a_malloc(size_t size);
void my_free(void* p);
void* my_realloc(void* p, size_t size);

void user_malloc_leak_bad() { int* x = (int*)a_malloc(sizeof(int)); }

void test_config_options_1_ok() {
  int* p = (int*)malloc(sizeof(int));
  int* q = my_realloc(p, sizeof(int));
  if (!q) {
    my_free(p);
    return;
  }
  my_free(q);
}

void test_config_options_2_ok() {
  int* p = (int*)my_malloc(sizeof(int));
  int* q = realloc(p, sizeof(int));
  if (!q) {
    my_free(p);
    return;
  }
  my_free(q);
}

void test_config_options_no_free_bad() {
  int* p = (int*)my_malloc(sizeof(int));
  int* q = my_realloc(p, sizeof(int));
}

struct ref_counted {
  size_t count;
  int data;
};

// the address of the malloc()'d pointer can be retrieved from the
// return value using pointer arithmetic
int* alloc_ref_counted_ok() {
  struct ref_counted* p =
      (struct ref_counted*)malloc(sizeof(struct ref_counted));
  if (p) {
    p->count = 1;
    return &(p->data);
  } else {
    return NULL;
  }
}

// returning the value of the field loses the address of p
int alloc_ref_counted_bad() {
  struct ref_counted* p =
      (struct ref_counted*)malloc(sizeof(struct ref_counted));
  if (p) {
    p->count = 1;
    p->data = 42;
    return p->data;
  } else {
    return 0;
  }
}

// the address of the malloc()'d pointer can be retrieved from the
// return value using pointer arithmetic
void* alloc_ref_counted_arith_ok(size_t size) {
  int* p = (int*)malloc(size + sizeof(int));
  if (p) {
    // register count = 1 and point past the ref count
    *p++ = 1;
  }
  return p;
}

int return_malloc_deref_bad() {
  int* p = (int*)malloc(sizeof(int));
  if (p) {
    *p = 42;
    return *p;
  }
  return 10;
}

typedef struct node_st {
  int* data;
} NODE;

void mutual_recursion_2(NODE* x);

void mutual_recursion(NODE* x) { mutual_recursion_2(x); }

void mutual_recursion_2(NODE* x) { mutual_recursion(x); }

void interproc_mutual_recusion_leak(NODE* x) {
  int* d;
  if (x->data == NULL) {
    x->data = (int*)malloc(sizeof(int));
  }
  mutual_recursion(x);
}

void allocate_all_in_array(int* array[]) {
  for (int i = 0; i < 2; i++) {
    array[i] = malloc(sizeof(int));
  }
}

void free_all_in_array(int* array[]) {
  for (int i = 0; i < 2; i++) {
    free(array[i]);
  }
}

void alloc_then_free_all_in_array_ok() {
  int* array[2];
  allocate_all_in_array(array);
  free_all_in_array(array);
}

void allocate_42_in_array(int* array[]) { array[42] = malloc(sizeof(int)); }

void free_42_in_array(int* array[]) { free(array[42]); }

void alloc_then_free_42_in_array_ok() {
  int* array[64];
  allocate_42_in_array(array);
  free_42_in_array(array);
}

void allocate_in_array(int* array[], int i) { array[i] = malloc(sizeof(int)); }

void free_in_array(int* array[], int i) { free(array[i]); }

void alloc_then_free_fixed_index_ok() {
  int* array[64];
  allocate_in_array(array, 42);
  free_in_array(array, 42);
}

void alloc_then_free_parameter_array_ok(int* array[], int i) {
  allocate_in_array(array, i);
  free_in_array(array, i);
}

void alloc_then_free_at_index_ok(int i) {
  int* array[64];
  allocate_in_array(array, i);
  free_in_array(array, i);
}

// unknown and cannot write through [p], so Pulse treats its calls as pure
int read_through_const_pointer(const int* p);

int pure_call_on_leaked_pointer_bad() {
  int* p = malloc(sizeof(int));
  if (p == NULL) {
    return 0;
  }
  return read_through_const_pointer(p);
}

void pure_call_on_leaked_pointer_result_stored_bad(int* out) {
  int* p = malloc(sizeof(int));
  if (p == NULL) {
    return;
  }
  *out = read_through_const_pointer(p);
}

int pure_call_on_freed_pointer_ok() {
  int* p = malloc(sizeof(int));
  if (p == NULL) {
    return 0;
  }
  int r = read_through_const_pointer(p);
  free(p);
  return r;
}

int pure_call_on_leaked_pointer_plus_one_bad() {
  int* p = malloc(sizeof(int));
  if (p == NULL) {
    return 0;
  }
  return read_through_const_pointer(p) + 1;
}

int pure_call_on_offset_of_leaked_pointer_bad() {
  int* p = malloc(2 * sizeof(int));
  if (p == NULL) {
    return 0;
  }
  return read_through_const_pointer(p + 1);
}

int read_through_const_pointer_at(int i, const int* p);

int pure_call_with_live_argument_on_leaked_pointer_bad(int i) {
  int* p = malloc(sizeof(int));
  if (p == NULL) {
    return 0;
  }
  return read_through_const_pointer_at(i, p);
}

int nested_pure_calls_on_leaked_pointer_bad() {
  int* p = malloc(sizeof(int));
  if (p == NULL) {
    return 0;
  }
  return read_through_const_pointer_at(read_through_const_pointer(p), p);
}

int two_pure_calls_on_leaked_pointer_bad() {
  int* p = malloc(sizeof(int));
  if (p == NULL) {
    return 0;
  }
  return read_through_const_pointer(p) + read_through_const_pointer_at(0, p);
}

int pure_size(void);

int pure_call_with_pure_argument_on_leaked_pointer_bad(int* out) {
  int* p = malloc(sizeof(int));
  if (p == NULL) {
    return 0;
  }
  int n = pure_size();
  *out = n;
  return read_through_const_pointer_at(n, p);
}

const int* find_through_const_pointer(const int* p);

// FP: Pulse does not know that the result of the unknown call can be [p]
const int* FP_pure_call_returning_its_argument_ok() {
  int* p = malloc(sizeof(int));
  if (p == NULL) {
    return NULL;
  }
  return find_through_const_pointer(p);
}
