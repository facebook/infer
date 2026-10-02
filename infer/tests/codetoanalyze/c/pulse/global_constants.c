/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>
#include <string.h>

// calls through function pointers stored in global constants

struct obj;

struct obj_ops {
  void (*release)(struct obj*);
};

struct obj {
  const struct obj_ops* ops;
  int val;
};

static void obj_free(struct obj* o) { free(o); }

static void obj_nop(struct obj* o) {}

static void (*const free_fn)(struct obj*) = obj_free;

static const struct obj_ops free_ops = {.release = obj_free};

static const struct obj_ops nop_ops = {.release = obj_nop};

static const struct obj_ops null_ops = {.release = NULL};

static const struct obj_ops ops_array[2] = {{.release = obj_nop},
                                            {.release = obj_free}};

struct ops_holder {
  const struct obj_ops* ops;
};

static const struct ops_holder free_ops_holder = {.ops = &free_ops};

static const struct obj_ops* const free_ops_ptr = &free_ops;

static struct obj_ops mutable_ops = {.release = obj_free};

void set_mutable_ops_to_nop() { mutable_ops.release = obj_nop; }

static struct obj* obj_alloc() {
  struct obj* o = malloc(sizeof(struct obj));
  if (o == NULL) {
    exit(1);
  }
  o->val = 0;
  return o;
}

int call_const_global_function_pointer_bad() {
  struct obj* o = obj_alloc();
  free_fn(o);
  return o->val;
}

int call_const_global_field_bad() {
  struct obj* o = obj_alloc();
  free_ops.release(o);
  return o->val;
}

int call_const_global_field_ok() {
  struct obj* o = obj_alloc();
  nop_ops.release(o);
  int v = o->val;
  free(o);
  return v;
}

int call_through_object_bad() {
  struct obj* o = obj_alloc();
  o->ops = &free_ops;
  o->ops->release(o);
  return o->val;
}

int call_through_object_ok() {
  struct obj* o = obj_alloc();
  o->ops = &nop_ops;
  o->ops->release(o);
  int v = o->val;
  free(o);
  return v;
}

static void obj_release(struct obj* o) { o->ops->release(o); }

int call_through_object_in_callee_bad() {
  struct obj* o = obj_alloc();
  o->ops = &free_ops;
  obj_release(o);
  return o->val;
}

static void set_free_ops(struct obj* o) { o->ops = &free_ops; }

int call_through_object_set_in_callee_bad() {
  struct obj* o = obj_alloc();
  set_free_ops(o);
  o->ops->release(o);
  return o->val;
}

static int init_free_ops(struct obj* o) {
  o->ops = &free_ops;
  return 0;
}

int call_through_object_initialized_in_callee_bad() {
  struct obj* o = obj_alloc();
  init_free_ops(o);
  o->ops->release(o);
  return o->val;
}

static void release_with_ops(const struct obj_ops* ops, struct obj* o) {
  ops->release(o);
}

static void release_with_free_ops(struct obj* o) {
  release_with_ops(&free_ops, o);
}

int call_through_argument_in_callee_bad() {
  struct obj* o = obj_alloc();
  release_with_free_ops(o);
  return o->val;
}

static const struct obj_ops* get_free_ops() { return &free_ops; }

int call_through_returned_pointer_bad() {
  struct obj* o = obj_alloc();
  get_free_ops()->release(o);
  return o->val;
}

int call_through_local_struct_bad() {
  struct obj* o = obj_alloc();
  struct ops_holder holder = {.ops = &free_ops};
  holder.ops->release(o);
  return o->val;
}

// the initializers of [free_ops_holder] and [free_ops_ptr] store the address of
// [free_ops] without inlining the initializer of [free_ops], so that the
// initializer of a constant does not inline all the constants reachable from it
void FN_call_through_nested_const_global_bad() {
  struct obj* o = obj_alloc();
  free_ops_holder.ops->release(o);
  free(o);
}

void FN_call_through_const_global_pointer_bad() {
  struct obj* o = obj_alloc();
  free_ops_ptr->release(o);
  free(o);
}

// the initializers of global constants are not inlined when accessing array
// elements, to avoid copying whole tables into the abstract state
void FN_call_const_array_element_bad() {
  struct obj* o = obj_alloc();
  ops_array[1].release(o);
  free(o);
}

int call_const_array_element_ok() {
  struct obj* o = obj_alloc();
  ops_array[0].release(o);
  int v = o->val;
  free(o);
  return v;
}

// non-const globals can be modified elsewhere, eg by [set_mutable_ops_to_nop]
void call_mutable_global_field_ok() {
  struct obj* o = obj_alloc();
  mutable_ops.release(o);
  free(o);
}

struct release_stats {
  int released;
};

static struct release_stats stats;

struct counted_ops {
  void (*release)(struct obj*);
  int* counter;
};

// taking the address of a field of a non-const global does not read it
static const struct counted_ops counted_free_ops = {.release = obj_free,
                                                    .counter = &stats.released};

int call_const_global_field_with_address_of_global_bad() {
  struct obj* o = obj_alloc();
  counted_free_ops.release(o);
  return o->val;
}

struct large_ops {
  void (*release)(struct obj*);
  int padding[64];
};

static const struct large_ops large_free_ops = {.release = obj_free};

// the initializers of constants of more than 64 scalars and pointers are not
// inlined when accessing their fields, to keep summaries small
void FN_call_large_const_global_field_bad() {
  struct obj* o = obj_alloc();
  large_free_ops.release(o);
  free(o);
}

void call_null_const_global_field_bad() {
  struct obj* o = obj_alloc();
  null_ops.release(o);
  free(o);
}

void call_null_const_global_field_checked_ok() {
  struct obj* o = obj_alloc();
  if (null_ops.release != NULL) {
    null_ops.release(o);
  }
  free(o);
}

// fields of global constants that are not function pointers

struct config {
  int enabled;
  int* ptr;
};

static const struct config config_off = {.enabled = 0, .ptr = NULL};

static const struct config config_on = {.enabled = 1, .ptr = NULL};

int read_const_global_field_ok() {
  if (config_off.enabled) {
    return *config_off.ptr;
  }
  return 0;
}

int read_const_global_field_bad() {
  if (config_on.enabled) {
    return *config_on.ptr;
  }
  return 0;
}

// volatile and weak constants can have other values than the ones given by
// their initializers

static const volatile struct config volatile_config = {.enabled = 0};

void volatile_const_struct_field_bad() {
  if (volatile_config.enabled) {
    int* p = NULL;
    *p = 42;
  }
}

static const volatile int volatile_flag = 0;

void volatile_const_global_bad() {
  if (volatile_flag) {
    int* p = NULL;
    *p = 42;
  }
}

__attribute__((weak)) const struct config weak_config = {.enabled = 0};

void weak_const_struct_field_bad() {
  if (weak_config.enabled) {
    int* p = NULL;
    *p = 42;
  }
}

__attribute__((weak)) const int weak_flag = 0;

void weak_const_global_bad() {
  if (weak_flag) {
    int* p = NULL;
    *p = 42;
  }
}

struct with_volatile_member {
  volatile int enabled;
};

static const struct with_volatile_member volatile_member = {.enabled = 0};

void volatile_member_const_struct_field_bad() {
  if (volatile_member.enabled) {
    int* p = NULL;
    *p = 42;
  }
}

struct with_nested_volatile_member {
  struct {
    volatile int counters[1];
  } inner;
  int enabled;
};

static const struct with_nested_volatile_member nested_volatile_config = {
    .enabled = 0};

void nested_volatile_member_const_struct_field_bad() {
  if (nested_volatile_config.enabled) {
    int* p = NULL;
    *p = 42;
  }
}

extern void unknown_write(void* p);

static const int const_one = 1;

// writing to a constant is undefined behaviour, so its value is restored
// after unknown code may have written to it
int read_const_global_after_unknown_write_ok() {
  unknown_write((void*)&const_one);
  if (const_one != 1) {
    int* p = NULL;
    return *p;
  }
  return 0;
}

// global constants whose initializers refer to each other

struct node {
  const struct node* next;
  int val;
};

static const struct node node_b;

static const struct node node_a = {.next = &node_b, .val = 1};

static const struct node node_b = {.next = &node_a, .val = 2};

void read_cyclic_const_globals_ok() {
  if (node_a.next != &node_b || node_b.next != &node_a) {
    int* p = NULL;
    *p = 42;
  }
}

// lookups in constant tables

struct group {
  const char* name;
  int sign;
};

static const struct group groups[] = {{"a", 1}, {"b", -1}, {"c", 1}, {"d", -1}};

static const int num_groups = 4;

static const struct group* lookup_group(const char* name,
                                        const struct group* gs,
                                        int n) {
  for (int i = 0; i < n; i++) {
    if (strcmp(gs[i].name, name) == 0) {
      return &gs[i];
    }
  }
  return NULL;
}

// Pulse only explores the first iterations of a loop, so it misses the lookup
// failing after going through the whole table, as with a literal bound
int FN_failed_lookup_in_const_table_bad(const char* name) {
  return lookup_group(name, groups, num_groups)->sign;
}

static int get_size(const char* name, const struct group* gs, int n) {
  return n;
}

// the value loaded from [num_groups] is kept when the initializer of [groups]
// is inlined for the next argument
int pass_const_table_and_size_ok(const char* name) {
  if (get_size(name, groups, num_groups) != 4) {
    int* p = NULL;
    return *p;
  }
  return 0;
}
