/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <string.h>

// functions without a body: their declarations are all we know about them
int nonnull_param(const char* _Nonnull s);
int nonnull_params_var_arg(const char* _Nonnull s,
                           const char* _Nonnull fmt,
                           ...);
void nullable_param(const char* _Nullable s);
void null_unspecified_param(const char* _Null_unspecified s);
void unannotated_param(const char* s);
void nonnull_attribute_second(const char* p, const char* q)
    __attribute__((nonnull(2)));
void nonnull_attribute_all(int n, const char* p, const char* q)
    __attribute__((nonnull));
void nonnull_attribute_on_param(const char* p __attribute__((nonnull)));
void nullable_param_nonnull_attribute(const char* _Nullable p)
    __attribute__((nonnull));
// glibc declares its functions with such a macro
#define __test_nonnull(params) __attribute__((__nonnull__ params))
void nonnull_macro(const char* p, const char* q) __test_nonnull((1));
int nonnull_array_param(int fds[_Nonnull 2]);
int nonnull_param_parenthesized(const char* _Nonnull(s));
struct object;
void nonnull_param_named_this(struct object* _Nonnull this);
int map_put(const char* _Nonnull key, void* _Nonnull value);
void forget_address(const void* _Nonnull p);
char* unknown_string(void);
// declared like glibc's qsort
void sort_like_qsort(void* base,
                     size_t n,
                     size_t size,
                     int (*compar)(const void*, const void*))
    __attribute__((nonnull(1, 4)));

typedef const char* _Nonnull nonnull_string;
typedef nonnull_string nonnull_string_alias;
typedef const char* _Nullable nullable_string;
void nonnull_typedef_param(nonnull_string s);
void nonnull_typedef_alias_param(nonnull_string_alias s);
void nullable_typedef_param(nullable_string s);

#pragma clang assume_nonnull begin
void assume_nonnull_param(const char* s);
void assume_nonnull_nullable_param(const char* _Nullable s);
#pragma clang assume_nonnull end

int null_to_nonnull_param_bad(void) {
  const char* s = NULL;
  return nonnull_param(s);
}

int getenv_to_nonnull_param_bad(void) { return nonnull_param(getenv("VAR")); }

int strchr_to_nonnull_param_bad(const char* line) {
  int v = 0;
  const char* eq = strchr(line, '=');
  nonnull_params_var_arg(eq, "=%d", &v);
  return v;
}

int checked_getenv_to_nonnull_param_ok(void) {
  const char* s = getenv("VAR");
  if (s == NULL) {
    return 0;
  }
  return nonnull_param(s);
}

int unknown_value_to_nonnull_param_ok(void) {
  return nonnull_param(unknown_string());
}

void null_to_nullable_param_ok(void) {
  const char* s = NULL;
  nullable_param(s);
}

void null_to_null_unspecified_param_ok(void) {
  const char* s = NULL;
  null_unspecified_param(s);
}

void null_to_unannotated_param_ok(void) {
  const char* s = NULL;
  unannotated_param(s);
}

void null_to_variadic_arg_ok(void) {
  const char* s = NULL;
  nonnull_params_var_arg("x", "%p", s);
}

void null_to_nonnull_attribute_index_bad(void) {
  const char* s = NULL;
  nonnull_attribute_second("x", s);
}

void null_to_param_not_in_nonnull_attribute_ok(void) {
  const char* s = NULL;
  nonnull_attribute_second(s, "x");
}

void null_to_nonnull_attribute_all_bad(void) {
  const char* s = NULL;
  nonnull_attribute_all(0, "x", s);
}

void null_to_nonnull_attribute_on_param_bad(void) {
  const char* s = NULL;
  nonnull_attribute_on_param(s);
}

void null_to_nullable_param_nonnull_attribute_ok(void) {
  const char* s = NULL;
  nullable_param_nonnull_attribute(s);
}

void null_to_nonnull_macro_bad(void) {
  const char* s = NULL;
  nonnull_macro(s, "x");
}

void null_to_param_not_in_nonnull_macro_ok(void) {
  const char* s = NULL;
  nonnull_macro("x", s);
}

int null_to_nonnull_array_param_bad(void) {
  int* fds = NULL;
  return nonnull_array_param(fds);
}

int null_to_nonnull_param_parenthesized_bad(void) {
  const char* s = NULL;
  return nonnull_param_parenthesized(s);
}

void null_to_nonnull_param_named_this_bad(void) {
  struct object* o = NULL;
  nonnull_param_named_this(o);
}

void null_to_nonnull_typedef_param_bad(void) {
  const char* s = NULL;
  nonnull_typedef_param(s);
}

void null_to_nonnull_typedef_alias_param_bad(void) {
  const char* s = NULL;
  nonnull_typedef_alias_param(s);
}

void null_to_nullable_typedef_param_ok(void) {
  const char* s = NULL;
  nullable_typedef_param(s);
}

void null_to_assume_nonnull_param_bad(void) {
  const char* s = NULL;
  assume_nonnull_param(s);
}

void null_to_assume_nonnull_nullable_param_ok(void) {
  const char* s = NULL;
  assume_nonnull_nullable_param(s);
}

static int compare_ints(const void* a, const void* b) {
  return *(const int*)a - *(const int*)b;
}

// passing null is undefined behaviour even when there are no elements to sort
void lazily_allocated_array_to_qsort_like_bad(int* array, size_t n) {
  int* items = NULL;
  if (n > 0) {
    items = array;
  }
  sort_like_qsort(items, n, sizeof(int), compare_ints);
}

int forward_to_nonnull_param(const char* s) { return nonnull_param(s); }

int null_to_forwarding_function_bad(void) {
  return forward_to_nonnull_param(NULL);
}

int forward_to_forwarding_function(const char* s) {
  return forward_to_nonnull_param(s);
}

int null_to_two_forwarding_functions_bad(void) {
  return forward_to_forwarding_function(NULL);
}

void put_value(const char* key, void* value) { map_put(key, value); }

void forward_to_put_value(const char* key, void* value) {
  put_value(key, value);
}

// any non-null value is accepted, so the analysis goes on after these calls
int sentinel_to_nonnull_param_then_null_deref_bad(void) {
  map_put("k", (void*)1);
  int* p = NULL;
  return *p;
}

int sentinel_to_forwarding_function_then_null_deref_bad(void) {
  put_value("k", (void*)1);
  int* p = NULL;
  return *p;
}

int sentinel_to_two_forwarding_functions_then_null_deref_bad(void) {
  forward_to_put_value("k", (void*)1);
  int* p = NULL;
  return *p;
}

void freed_to_nonnull_param_ok(void) {
  char* s = (char*)malloc(2);
  if (s == NULL) {
    return;
  }
  free(s);
  forget_address(s);
}

void forward_to_nonnull_param_then_write(char* s) {
  nonnull_param(s);
  *s = 'a';
}

void freed_to_forwarding_function_then_write_bad(void) {
  char* s = (char*)malloc(2);
  if (s == NULL) {
    return;
  }
  free(s);
  forward_to_nonnull_param_then_write(s);
}

void nonnull_attribute_param(int* p) __attribute__((nonnull));

void nonnull_attribute_then_write_alias(int* p, int* q) {
  nonnull_attribute_param(p);
  *q = 1;
  if (p == q) {
    unannotated_param("p == q");
  }
}

// the requirement that [p] is not null does not replace the one that [q] is
// valid when they turn out to be the same pointer
void freed_to_nonnull_attribute_then_write_alias_bad(void) {
  int* x = (int*)malloc(sizeof(int));
  if (x == NULL) {
    return;
  }
  free(x);
  nonnull_attribute_then_write_alias(x, x);
}

// reported as COMPARED_TO_NULL_AND_DEREFERENCED, which is disabled by default
void compared_to_null_then_nonnull_param_bad(const char* s) {
  if (s == NULL) {
    unannotated_param("null");
  }
  nonnull_param(s);
}

// unlike with [!s] below, the requirement is not passed on to callers
void FN_null_to_compared_to_null_then_nonnull_param_bad(void) {
  compared_to_null_then_nonnull_param_bad(NULL);
}

void negated_to_nonnull_param(const char* s) {
  if (!s) {
    unannotated_param("null");
  }
  nonnull_param(s);
}

void null_to_negated_then_nonnull_param_bad(void) {
  negated_to_nonnull_param(NULL);
}

void nonnull_param_and_int(const char* _Nonnull s, int n);

void negated_to_nonnull_param_with_zero(const char* s) {
  if (!s) {
    unannotated_param("null");
  }
  nonnull_param_and_int(s, 0);
}

// on the null branch of [negated_to_nonnull_param_with_zero], [s] and the
// literal 0 are the same value, so the report is about a constant in the callee
// and is suppressed
void FN_null_to_negated_then_nonnull_param_with_zero_bad(void) {
  negated_to_nonnull_param_with_zero(NULL);
}

void redeclared_nonnull(const char* p);
void redeclared_nonnull(const char* p) __attribute__((nonnull));

void null_to_redeclared_nonnull_bad(void) {
  const char* s = NULL;
  redeclared_nonnull(s);
}

void later_declared_nonnull(const char* p);

void call_before_nonnull_declaration_ok(void) { later_declared_nonnull("x"); }

void later_declared_nonnull(const char* p) __attribute__((nonnull));

// the declaration used is the one visible at the first call in the file
void FN_null_to_later_declared_nonnull_bad(void) {
  const char* s = NULL;
  later_declared_nonnull(s);
}

void declared_nonnull_in_one_file(const char* p) __attribute__((nonnull));

// nonnull_annotation_other_file.c declares the function without the attribute
// and only one of the declarations is kept, here the other one
void FN_null_to_declared_nonnull_in_one_file_bad(void) {
  const char* s = NULL;
  declared_nonnull_in_one_file(s);
}

// functions with a body are judged by what their body does
int defined_nonnull_param_checked(const char* _Nonnull s) {
  if (s == NULL) {
    return 0;
  }
  return 1;
}

int null_to_defined_nonnull_param_ok(void) {
  const char* s = NULL;
  return defined_nonnull_param_checked(s);
}

void nonnull_attribute_var_arg(const char* fmt, ...) __attribute__((nonnull));

// the nonnull attribute without indexes also covers the pointers passed as
// variadic arguments, but only the declared parameters are checked
void FN_null_to_variadic_arg_of_nonnull_attribute_bad(void) {
  const char* s = NULL;
  nonnull_attribute_var_arg("x", s);
}
