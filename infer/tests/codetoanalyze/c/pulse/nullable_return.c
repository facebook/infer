/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <string.h>

// c/pulse-no-nullability-annotations analyzes this file without
// --pulse-nullability-annotations

struct item {
  int data;
};

// declared but not defined: calls to these functions are unknown to Pulse
struct item* _Nullable find_item(int key);
struct item* _Nullable_result find_item_result(int key);
struct item* _Nonnull get_item(int key);
struct item* get_item_attribute(int key) __attribute__((returns_nonnull));
[[gnu::returns_nonnull]] struct item* get_item_gnu_attribute(int key);
struct item* _Null_unspecified find_item_unspecified(int key);
struct item* find_item_unannotated(int key);
int (*_Nullable find_handler(int key))(void);
char* _Nullable find_name(int key);
typedef struct item* _Nullable nullable_item;
nullable_item find_item_typedef(int key);
typedef nullable_item nullable_item_alias;
nullable_item_alias find_item_two_level_typedef(int key);
typedef struct item* _Nonnull nonnull_item;
nonnull_item get_item_typedef(int key);
typedef struct item* _Nullable item_finder(int key);
item_finder find_item_function_typedef;
typedef struct item* _Nonnull item_getter(int key);
item_getter get_item_function_typedef;
__typeof__(find_item) find_item_typeof;
struct item* _Null_unspecified get_item_unspecified_attribute(int key)
    __attribute__((returns_nonnull));
struct item* _Nullable next_item(void* iterator);
char* _Nullable next_token(char* _Nullable str, const char* _Nonnull delim);

#pragma clang assume_nonnull begin
struct item* get_item_assumed(int key);
#pragma clang assume_nonnull end

int nullable_return_deref_bad(int key) { return find_item(key)->data; }

int nullable_result_return_deref_bad(int key) {
  return find_item_result(key)->data;
}

int nullable_function_pointer_call_bad(int key) { return find_handler(key)(); }

size_t nullable_return_strlen_bad(int key) { return strlen(find_name(key)); }

int deref_item(struct item* item) { return item->data; }

int nullable_return_passed_to_callee_bad(int key) {
  return deref_item(find_item(key));
}

struct item* _Nullable find_item_wrapper(int key) { return find_item(key); }

int nullable_return_through_wrapper_bad(int key) {
  return find_item_wrapper(key)->data;
}

int nullable_return_checked_ok(int key) {
  struct item* item = find_item(key);
  if (item == NULL) {
    return -1;
  }
  return item->data;
}

int nullable_return_called_twice_ok(int key) {
  if (find_item(key) != NULL) {
    return find_item(key)->data;
  }
  return -1;
}

int nullable_return_loop_ok(void* iterator) {
  int sum = 0;
  struct item* item;
  while ((item = next_item(iterator)) != NULL) {
    sum += item->data;
  }
  return sum;
}

int nullable_return_tokenizer_loop_ok(char* line) {
  int n = 0;
  for (char* t = next_token(line, " "); t != NULL; t = next_token(NULL, " ")) {
    n += t[0];
  }
  return n;
}

int nonnull_return_null_branch_ok(int key) {
  struct item* item = get_item(key);
  if (item == NULL) {
    int* leaked = malloc(sizeof(int));
    return -1;
  }
  return item->data;
}

int nonnull_attribute_return_null_branch_ok(int key) {
  struct item* item = get_item_attribute(key);
  if (item == NULL) {
    int* leaked = malloc(sizeof(int));
    return -1;
  }
  return item->data;
}

int gnu_nonnull_attribute_return_null_branch_ok(int key) {
  struct item* item = get_item_gnu_attribute(key);
  if (item == NULL) {
    int* leaked = malloc(sizeof(int));
    return -1;
  }
  return item->data;
}

int assume_nonnull_return_null_branch_ok(int key) {
  struct item* item = get_item_assumed(key);
  if (item == NULL) {
    int* leaked = malloc(sizeof(int));
    return -1;
  }
  return item->data;
}

int nullable_typedef_return_deref_bad(int key) {
  return find_item_typedef(key)->data;
}

int nullable_two_level_typedef_return_deref_bad(int key) {
  return find_item_two_level_typedef(key)->data;
}

int nonnull_typedef_return_null_branch_ok(int key) {
  struct item* item = get_item_typedef(key);
  if (item == NULL) {
    int* leaked = malloc(sizeof(int));
    return -1;
  }
  return item->data;
}

int nullable_function_typedef_return_deref_bad(int key) {
  return find_item_function_typedef(key)->data;
}

int nullable_typeof_declared_return_deref_bad(int key) {
  return find_item_typeof(key)->data;
}

int nonnull_function_typedef_return_null_branch_ok(int key) {
  struct item* item = get_item_function_typedef(key);
  if (item == NULL) {
    int* leaked = malloc(sizeof(int));
    return -1;
  }
  return item->data;
}

int null_unspecified_nonnull_attribute_return_null_branch_ok(int key) {
  struct item* item = get_item_unspecified_attribute(key);
  if (item == NULL) {
    int* leaked = malloc(sizeof(int));
    return -1;
  }
  return item->data;
}

int null_unspecified_return_deref_ok(int key) {
  return find_item_unspecified(key)->data;
}

int null_unspecified_return_null_branch_bad(int key) {
  struct item* item = find_item_unspecified(key);
  if (item == NULL) {
    int* leaked = malloc(sizeof(int));
    return -1;
  }
  return item->data;
}

int unannotated_return_deref_ok(int key) {
  return find_item_unannotated(key)->data;
}

static struct item global_item;

// defined, so Pulse uses its summary rather than the annotation
struct item* _Nullable defined_never_null(void) { return &global_item; }

int defined_nullable_function_ok(void) { return defined_never_null()->data; }

// skipped by --pulse-skip-procedures in the Makefile
struct item* _Nullable nullable_return_skipped(void) { return &global_item; }

int skipped_nullable_function_ok(void) {
  return nullable_return_skipped()->data;
}

struct item* _Nullable even_walk(struct item* item, int depth);

struct item* _Nullable odd_walk(struct item* item, int depth) {
  return depth <= 0 ? item : even_walk(item, depth - 1);
}

struct item* _Nullable even_walk(struct item* item, int depth) {
  return depth <= 0 ? item : odd_walk(item, depth - 1);
}

int mutually_recursive_nullable_functions_ok(struct item* item) {
  return even_walk(item, 3)->data;
}

// the recursive call has no summary yet, and Pulse does not use the
// annotations of defined functions
struct item* _Nullable FN_recursive_nullable_return_deref_bad(int depth) {
  if (depth == 0) {
    return NULL;
  }
  struct item* item = FN_recursive_nullable_return_deref_bad(depth - 1);
  item->data = depth;
  return item;
}

struct item* _Nullable cache_get(int key);
void cache_put(int key, struct item* item);
struct item* make_item(int key);

// Pulse assumes that the second call to cache_get returns the same value as
// the first one, as their arguments are equal
int FP_lazy_insert_then_lookup_ok(int key) {
  if (cache_get(key) == NULL) {
    cache_put(key, make_item(key));
  }
  return cache_get(key)->data;
}

int has_item(int key);

// Pulse does not know that find_item returns non-null when has_item returns
// non-zero
int FP_has_then_find_ok(int key) {
  if (has_item(key)) {
    return find_item(key)->data;
  }
  return -1;
}

// POSIX functions, declared with the nullability given by some C libraries
void* _Nullable dlsym(void* handle, const char* symbol);
char* _Nullable dlerror(void);

// Pulse does not know that dlsym returned non-null when dlerror returns null
int FP_dlsym_checked_with_dlerror_ok(void* handle) {
  dlerror();
  int (*f)(void) = (int (*)(void))dlsym(handle, "f");
  if (dlerror() != NULL) {
    return -1;
  }
  return f();
}
