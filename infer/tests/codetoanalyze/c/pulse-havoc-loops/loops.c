/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

// Loops that need more iterations to exit than --pulse-widen-threshold
// allows. With --pulse-havoc-interrupted-loops, the code after them is
// analyzed from states where what the loop body may modify is havoced.

int global_array[16];

void npe_after_constant_loop_bad() {
  for (int i = 0; i < 16; i++) {
    global_array[i] = i;
  }
  int* p = NULL;
  *p = 42;
}

void npe_after_countdown_loop_bad() {
  int n = 10;
  while (n > 0) {
    n--;
  }
  int* p = NULL;
  *p = 42;
}

void npe_after_nested_constant_loops_bad() {
  for (int i = 0; i < 8; i++) {
    for (int j = 0; j < 8; j++) {
      global_array[(i + j) % 16] = 0;
    }
  }
  int* p = NULL;
  *p = 42;
}

void npe_after_infinite_loop_with_break_bad() {
  int i = 0;
  for (;;) {
    if (i >= 16) {
      break;
    }
    i++;
  }
  int* p = NULL;
  *p = 42;
}

void use_after_free_after_constant_loop_bad(int* x) {
  free(x);
  for (int i = 0; i < 8; i++) {
    global_array[i] = 0;
  }
  *x = 42;
}

void free_in_first_iteration_then_use_bad(int* x) {
  for (int i = 0; i < 8; i++) {
    if (i == 0) {
      free(x);
    }
  }
  *x = 42;
}

void leak_after_constant_loop_bad() {
  int* p = (int*)malloc(sizeof(int));
  for (int i = 0; i < 8; i++) {
    global_array[i] = 0;
  }
}

int* constant_loop_then_return_null() {
  for (int i = 0; i < 8; i++) {
    global_array[i] = 0;
  }
  return NULL;
}

void npe_after_call_to_constant_loop_bad() {
  int* p = constant_loop_then_return_null();
  *p = 42;
}

int* symbolic_loop_then_return_null(int n) {
  for (int i = 0; i < n; i++) {
    global_array[i % 16] = 0;
  }
  return NULL;
}

void npe_after_call_to_symbolic_loop_with_large_bound_bad() {
  int* p = symbolic_loop_then_return_null(10);
  *p = 42;
}

int* global_pointers[16];
int global_target;

static void fill_global_pointers() {
  for (int i = 0; i < 16; i++) {
    global_pointers[i] = &global_target;
  }
}

// the element is not one of those that the unrolled iterations of the callee
// write
void caller_of_global_filler_ok() {
  global_pointers[10] = NULL;
  fill_global_pointers();
  *(global_pointers[10]) = 42;
}

static void set_to_global_target(int** slot) { *slot = &global_target; }

static void initialize_global_pointers() {
  for (int i = 0; i < 16; i++) {
    set_to_global_target(&global_pointers[i]);
  }
}

void caller_of_global_initializer_ok() {
  global_pointers[10] = NULL;
  initialize_global_pointers();
  *(global_pointers[10]) = 42;
}

int* late_global;

static void set_late_global() {
  for (int i = 0; i < 16; i++) {
    if (i == 7) {
      late_global = &global_target;
    }
  }
}

// the unrolled iterations of the callee do not access the global
void caller_of_late_global_setter_ok() {
  late_global = NULL;
  set_late_global();
  *late_global = 42;
}

int* late_pointers[16];

static void fill_upper_half_of_late_pointers() {
  for (int i = 0; i < 16; i++) {
    if (i >= 8) {
      late_pointers[i] = &global_target;
    }
  }
}

void caller_of_upper_half_filler_ok() {
  late_pointers[10] = NULL;
  fill_upper_half_of_late_pointers();
  *(late_pointers[10]) = 42;
}

int* late_global_used_after_loop;

static void set_late_global_then_use_it() {
  for (int i = 0; i < 16; i++) {
    if (i == 7) {
      late_global_used_after_loop = &global_target;
    }
  }
  *late_global_used_after_loop = 1;
}

void caller_of_late_global_setter_then_user_ok() {
  late_global_used_after_loop = NULL;
  set_late_global_then_use_it();
}

struct pointer_pair {
  int* first;
  int* second;
};

struct pointer_pair late_pair;

static void set_late_field_then_use_it() {
  for (int i = 0; i < 16; i++) {
    if (i == 7) {
      late_pair.second = &global_target;
    }
  }
  *(late_pair.second) = 1;
}

void caller_of_late_field_setter_then_user_ok() {
  late_pair.second = NULL;
  set_late_field_then_use_it();
}

static void set_late_field_through_parameter_then_use_it(
    struct pointer_pair* pair) {
  for (int i = 0; i < 16; i++) {
    if (i == 7) {
      pair->second = &global_target;
    }
  }
  *(pair->second) = 1;
}

void caller_of_late_field_setter_through_parameter_ok() {
  struct pointer_pair pair = {NULL, NULL};
  set_late_field_through_parameter_then_use_it(&pair);
}

int* late_aliased_global;

static void set_late_global_through_alias_then_use_it() {
  int** alias = &late_aliased_global;
  for (int i = 0; i < 16; i++) {
    if (i == 7) {
      *alias = &global_target;
    }
  }
  *late_aliased_global = 1;
}

void caller_of_late_global_setter_through_alias_ok() {
  late_aliased_global = NULL;
  set_late_global_through_alias_then_use_it();
}

struct pointer_pair* late_pair_pointer;

static void set_late_field_through_global_pointer_then_use_it() {
  for (int i = 0; i < 16; i++) {
    if (i == 7) {
      late_pair_pointer->second = &global_target;
    }
  }
  *(late_pair_pointer->second) = 1;
}

void caller_of_late_field_setter_through_global_pointer_ok(
    struct pointer_pair* pair) {
  pair->second = NULL;
  late_pair_pointer = pair;
  set_late_field_through_global_pointer_then_use_it();
}

struct counter {
  int count;
};

static void count_if_enabled(struct counter* counter, int enabled) {
  for (int i = 0; i < 16; i++) {
    if (enabled) {
      counter->count++;
    }
  }
}

void npe_after_disabled_counting_with_null_counter_bad() {
  count_if_enabled(NULL, 0);
  int* p = NULL;
  *p = 42;
}

struct counter* global_counter;

static void count_in_global_if_enabled(int enabled) {
  for (int i = 0; i < 16; i++) {
    if (enabled) {
      global_counter->count++;
    }
  }
}

void npe_after_disabled_counting_with_null_global_counter_bad() {
  global_counter = NULL;
  count_in_global_if_enabled(0);
  int* p = NULL;
  *p = 42;
}

static void set_late_if_enabled(int* flag, int enabled) {
  for (int i = 0; i < 16; i++) {
    if (enabled && i == 7) {
      *flag = 1;
    }
  }
}

void npe_after_disabled_setting_with_null_flag_bad() {
  set_late_if_enabled(NULL, 0);
  int* p = NULL;
  *p = 42;
}

static void count_if_enabled_then_reset(struct counter* counter, int enabled) {
  for (int i = 0; i < 16; i++) {
    if (enabled) {
      counter->count++;
    }
  }
  if (counter != NULL) {
    counter->count = 0;
  }
}

void npe_after_disabled_counting_with_null_checked_counter_bad() {
  count_if_enabled_then_reset(NULL, 0);
  int* p = NULL;
  *p = 42;
}

static void count_in_global_if_enabled_then_reset(int enabled) {
  for (int i = 0; i < 16; i++) {
    if (enabled) {
      global_counter->count++;
    }
  }
  if (global_counter != NULL) {
    global_counter->count = 0;
  }
}

void npe_after_disabled_counting_with_null_checked_global_counter_bad() {
  global_counter = NULL;
  count_in_global_if_enabled_then_reset(0);
  int* p = NULL;
  *p = 42;
}

static void set_late_if_enabled_then_reset(int* flag, int enabled) {
  for (int i = 0; i < 16; i++) {
    if (enabled && i == 7) {
      *flag = 1;
    }
  }
  if (flag != NULL) {
    *flag = 0;
  }
}

void npe_after_disabled_setting_with_null_checked_flag_bad() {
  set_late_if_enabled_then_reset(NULL, 0);
  int* p = NULL;
  *p = 42;
}

static void set_late_field_if_not_null_then_use_it(struct pointer_pair* pair) {
  for (int i = 0; i < 16; i++) {
    if (i == 7 && pair != NULL) {
      pair->second = &global_target;
    }
  }
  if (pair != NULL) {
    *(pair->second) = 1;
  }
}

void caller_of_checked_late_field_setter_ok() {
  struct pointer_pair pair = {NULL, NULL};
  set_late_field_if_not_null_then_use_it(&pair);
}

void npe_after_checked_late_field_setter_with_null_bad() {
  set_late_field_if_not_null_then_use_it(NULL);
  int* p = NULL;
  *p = 42;
}

static void set_if_enabled_then_reset_three(int* a,
                                            int* b,
                                            int* c,
                                            int enabled) {
  for (int i = 0; i < 16; i++) {
    if (enabled) {
      *a = i;
      *b = i;
      *c = i;
    }
  }
  if (a != NULL) {
    *a = 0;
  }
  if (b != NULL) {
    *b = 0;
  }
  if (c != NULL) {
    *c = 0;
  }
}

void npe_after_disabled_setting_with_first_of_three_null_bad() {
  int x, y;
  set_if_enabled_then_reset_three(NULL, &x, &y, 0);
  int* p = NULL;
  *p = 42;
}

// only two of the pointers written through in the loop are assumed to be
// possibly null after it: the callee's paths where `c` is null are lost
void FN_npe_after_disabled_setting_with_third_of_three_null_bad() {
  int x, y;
  set_if_enabled_then_reset_three(&x, &y, NULL, 0);
  int* p = NULL;
  *p = 42;
}

struct int_pair {
  int first;
  int second;
};

void null_branch_after_loop_on_local_field_address_ok(int enabled) {
  struct int_pair pair;
  int* p = &pair.second;
  for (int i = 0; i < 16; i++) {
    if (enabled) {
      *p = i;
    }
  }
  if (p == NULL) {
    int* q = NULL;
    *q = 42;
  }
}

int* unknown_pointer(void);

void null_branch_after_loop_on_unknown_pointer_ok(int enabled) {
  int* p = unknown_pointer();
  for (int i = 0; i < 16; i++) {
    if (enabled) {
      *p = i;
    }
  }
  if (p == NULL) {
    int* q = NULL;
    *q = 42;
  }
}

static void set_second_if_enabled_then_reset(struct int_pair* pair,
                                             int enabled) {
  int* p = &pair->second;
  for (int i = 0; i < 16; i++) {
    if (enabled) {
      *p = i;
    }
  }
  *p = 0;
}

void caller_of_field_address_setter_ok(int enabled) {
  struct int_pair pair;
  set_second_if_enabled_then_reset(&pair, enabled);
}

struct pair_holder {
  struct pointer_pair* pair;
};

static void set_late_nested_field_then_use_it(struct pair_holder* holder) {
  for (int i = 0; i < 16; i++) {
    if (i == 7) {
      holder->pair->second = &global_target;
    }
  }
  *(holder->pair->second) = 1;
}

// the field is written through a pointer loaded from memory: the cell keeps
// the value of the caller
void FP_caller_of_late_nested_field_setter_then_user_ok() {
  struct pointer_pair pair = {NULL, NULL};
  struct pair_holder holder = {&pair};
  set_late_nested_field_then_use_it(&holder);
}

int* filled_then_used_pointers[16];

static void fill_global_pointers_then_use_one() {
  for (int i = 0; i < 16; i++) {
    filled_then_used_pointers[i] = &global_target;
  }
  *(filled_then_used_pointers[10]) = 1;
}

// the element read after the loop is not one of those that the unrolled
// iterations write, so its value is that of the caller
void FP_caller_of_global_array_filler_then_user_ok() {
  filled_then_used_pointers[10] = NULL;
  fill_global_pointers_then_use_one();
}

int* global_assigned_through_late_alias;

static void set_global_through_late_alias_then_use_it() {
  int** alias = NULL;
  for (int i = 0; i < 16; i++) {
    if (i == 6) {
      alias = &global_assigned_through_late_alias;
    }
    if (i == 7) {
      *alias = &global_target;
    }
  }
  *global_assigned_through_late_alias = 1;
}

// the global, whose address is taken in the loop, is havoced like a struct or
// array: the cells that the explored iterations did not access keep the value
// of the caller
void FP_caller_of_global_setter_through_late_alias_ok() {
  global_assigned_through_late_alias = NULL;
  set_global_through_late_alias_then_use_it();
}

int* late_escaped_global;

static void initialize_late_global_then_use_it() {
  for (int i = 0; i < 16; i++) {
    if (i == 7) {
      set_to_global_target(&late_escaped_global);
    }
  }
  *late_escaped_global = 1;
}

void caller_of_late_global_initializer_ok() {
  late_escaped_global = NULL;
  initialize_late_global_then_use_it();
  *late_escaped_global = 42;
}

struct state {
  int value;
};

struct state* current_state;

static void reset_current_state() {
  for (int i = 0; i < 16; i++) {
    current_state = NULL;
  }
}

void object_stored_in_global_pointer_ok() {
  struct state* o = (struct state*)malloc(sizeof(struct state));
  if (o == NULL) {
    return;
  }
  o->value = 0;
  current_state = o;
  reset_current_state();
  int x;
  int* p = (o->value == 0) ? &x : NULL;
  *p = 1;
  free(o);
}

struct state* state_table[16];

static void clear_state_table() {
  for (int i = 0; i < 16; i++) {
    state_table[i] = NULL;
  }
}

// callers also havoc the memory reachable from a struct or array global
// assigned in a cut loop
void FP_object_stored_in_global_array_ok() {
  struct state* o = (struct state*)malloc(sizeof(struct state));
  if (o == NULL) {
    return;
  }
  o->value = 0;
  state_table[10] = o;
  clear_state_table();
  int x;
  int* p = (o->value == 0) ? &x : NULL;
  *p = 1;
  free(o);
}

struct s {
  int* f;
  int n;
};

static int get_n(const struct s* s) { return s->n; }

void npe_after_loop_with_const_argument_bad() {
  struct s s = {NULL, 0};
  for (int i = 0; i < 10; i++) {
    get_n(&s);
  }
  *(s.f) = 42;
}

void late_assignment_in_loop_ok() {
  int x = 0;
  int* p = NULL;
  for (int i = 0; i < 10; i++) {
    if (i == 7) {
      p = &x;
    }
  }
  *p = 42;
}

void late_write_through_pointer_in_loop_ok(struct s* s) {
  int x = 0;
  s->f = NULL;
  for (int i = 0; i < 10; i++) {
    if (i == 7) {
      s->f = &x;
    }
  }
  *(s->f) = 42;
}

static void set_field(struct s* s, int* v) { s->f = v; }

void late_write_by_callee_in_loop_ok() {
  int x = 0;
  struct s s = {NULL};
  for (int i = 0; i < 10; i++) {
    if (i == 7) {
      set_field(&s, &x);
    }
  }
  *(s.f) = 42;
}

void late_write_through_alias_in_loop_ok() {
  int x = 0;
  int* p = NULL;
  int** alias = NULL;
  for (int i = 0; i < 10; i++) {
    if (i == 6) {
      alias = &p;
    }
    if (i == 7) {
      *alias = &x;
    }
  }
  *p = 42;
}

struct entry {
  int* p;
};

void late_write_through_element_pointer_in_loop_ok(struct entry* table) {
  int x = 0;
  table[7].p = NULL;
  for (int i = 0; i < 16; i++) {
    struct entry* e = &table[i];
    e->p = &x;
  }
  *(table[7].p) = 42;
}

struct holder {
  struct s* s;
};

void late_write_through_pointer_in_struct_in_loop_ok(struct s* s) {
  int x = 0;
  struct holder h = {s};
  s->f = NULL;
  for (int i = 0; i < 10; i++) {
    if (i == 7) {
      h.s->f = &x;
    }
  }
  *(s->f) = 42;
}

struct s* check_correlated_across_loop_assigning_pointer_ok(struct s* s) {
  int x = 0;
  int* p = NULL;
  if (s->n) {
    p = &x;
  }
  struct s* cur = NULL;
  for (int i = 0; i < 16; i++) {
    cur = s;
  }
  if (s->n) {
    *p = 42;
  }
  return cur;
}

void store_or_free_in_loop_no_leak_ok(int* keys, int k, void** slot) {
  void* n = malloc(sizeof(int));
  if (n == NULL) {
    return;
  }
  int stored = 0;
  for (int i = 0; i < 16; i++) {
    if (!stored && keys[i] == k) {
      *slot = n;
      stored = 1;
    }
  }
  if (!stored) {
    free(n);
  }
}

void free_and_allocate_in_loop_no_leak_ok() {
  int* p = NULL;
  for (int i = 0; i < 8; i++) {
    free(p);
    p = (int*)malloc(sizeof(int));
  }
  free(p);
}

void fill_then_free_in_constant_loops_ok() {
  int* arr[8];
  for (int i = 0; i < 8; i++) {
    arr[i] = (int*)malloc(sizeof(int));
  }
  for (int i = 0; i < 8; i++) {
    free(arr[i]);
  }
}

struct node {
  struct node* next;
};

// the traversal pointer stays attached to the list, so [elem] remains
// reachable from [head]
static void append(struct node* head, struct node* elem) {
  struct node* it = head;
  while (it->next != NULL) {
    it = it->next;
  }
  it->next = elem;
}

void list_traversal_then_append_no_leak_ok(struct node* head) {
  struct node* elem = (struct node*)calloc(1, sizeof(struct node));
  if (elem == NULL) {
    return;
  }
  append(head, elem);
}

static void append_with_next_pointer(struct node* head, struct node* elem) {
  struct node* it = head;
  struct node* next;
  while ((next = it->next) != NULL) {
    it = next;
  }
  it->next = elem;
}

void list_traversal_with_next_pointer_then_append_no_leak_ok(
    struct node* head) {
  struct node* elem = (struct node*)calloc(1, sizeof(struct node));
  if (elem == NULL) {
    return;
  }
  append_with_next_pointer(head, elem);
}

static struct node* next_of(struct node* n) { return n->next; }

static void append_with_accessor(struct node* head, struct node* elem) {
  struct node* it = head;
  while (next_of(it) != NULL) {
    it = next_of(it);
  }
  it->next = elem;
}

void list_traversal_with_accessor_then_append_no_leak_ok(struct node* head) {
  struct node* elem = (struct node*)calloc(1, sizeof(struct node));
  if (elem == NULL) {
    return;
  }
  append_with_accessor(head, elem);
}

void npe_after_infinite_loop_ok() {
  int i = 0;
  while (i < 10) {
    global_array[0] = 1;
  }
  int* p = NULL;
  *p = 42;
}

int rand_int(void);

static int many_paths(void) {
  switch (rand_int()) {
    case 0:
      return 1;
    case 1:
      return 2;
    case 2:
      return 3;
    case 3:
      return 4;
    case 4:
      return 5;
    case 5:
      return 6;
    case 6:
      return 7;
    case 7:
      return 8;
    case 8:
      return 9;
    case 9:
      return 10;
    case 10:
      return 11;
    case 11:
      return 12;
    case 12:
      return 13;
    case 13:
      return 14;
    case 14:
      return 15;
    case 15:
      return 16;
    case 16:
      return 17;
    case 17:
      return 18;
    case 18:
      return 19;
    case 19:
      return 20;
    default:
      return 0;
  }
}

// the call gives more states than the disjunct limit: the states that left
// the loop by its exit test, such as the one where [i == 0], are kept before
// the havoced ones
void npe_on_first_exit_before_call_with_many_paths_bad(int* a) {
  int i = 0;
  while (i < 255 && a[i + 1] == a[0]) {
    i++;
  }
  many_paths();
  if (i == 0) {
    int* p = NULL;
    *p = 42;
  }
}

static int count_random_steps(void) {
  int i = 0;
  while (i < 255 && rand_int()) {
    i++;
  }
  return i;
}

// same with the loop in a callee: the havoced post of its summary is kept
// last in the caller too
void npe_on_first_exit_in_callee_before_call_with_many_paths_bad() {
  int i = count_random_steps();
  many_paths();
  if (i == 0) {
    int* p = NULL;
    *p = 42;
  }
}

static int count_random_steps_then_call_with_many_paths(void) {
  int i = 0;
  while (i < 255 && rand_int()) {
    i++;
  }
  many_paths();
  return i;
}

void npe_on_first_exit_in_callee_with_many_paths_bad() {
  int i = count_random_steps_then_call_with_many_paths();
  many_paths();
  if (i == 0) {
    int* p = NULL;
    *p = 42;
  }
}

struct map;

struct record {
  int id;
};

void map_insert(struct map* m, int key);

int map_contains(struct map* m, int key);

static struct record* find_or_null(struct map* m, int key, struct record* r) {
  if (!map_contains(m, key)) {
    return NULL;
  }
  return r;
}

// the lookup of a key inserted by an unknown call may fail; this is also
// reported without the option when the loop is short
void FP_lookup_after_inserting_in_loop_ok(struct map* m, struct record* r) {
  for (int i = 0; i < 16; i++) {
    map_insert(m, i);
  }
  find_or_null(m, 3, r)->id = 0;
}

// the counter is havoced rather than set to its final value
void FP_exact_counter_after_loop_ok() {
  int i;
  int x = 0;
  for (i = 0; i < 10; i++) {
  }
  int* p = (i == 10) ? &x : NULL;
  *p = 42;
}

// accumulators are havoced too
void FP_accumulator_after_loop_ok() {
  int sum = 0;
  int x = 0;
  for (int i = 0; i < 10; i++) {
    sum += i;
  }
  int* p = (sum == 45) ? &x : NULL;
  *p = 42;
}

int* global_pointer;

static void set_global_pointer(int* v) { global_pointer = v; }

// only the variables written in the loop body itself are havoced, not the
// globals written by callees
void FP_late_write_of_global_by_callee_in_loop_ok() {
  int x = 0;
  global_pointer = NULL;
  for (int i = 0; i < 10; i++) {
    if (i == 7) {
      set_global_pointer(&x);
    }
  }
  *global_pointer = 42;
}

// [p] is only updated from its own value so it keeps the value it had when the
// loop was cut, while the counter is havoced
void FP_pointer_compared_after_counter_loop_ok() {
  char buf[16];
  char* p = buf;
  for (int i = 0; i < 16; i++) {
    *p = 0;
    p++;
  }
  if (p != buf + 16) {
    int* q = NULL;
    *q = 42;
  }
}

// [p] keeps the value it had when the loop was cut, and the elements written
// through it in the remaining iterations keep their values too
void FP_array_filled_through_incremented_pointer_ok() {
  int* buf[16];
  int x = 0;
  buf[10] = NULL;
  int** p = buf;
  for (int i = 0; i < 16; i++) {
    *p = &x;
    p++;
  }
  *(buf[10]) = 42;
}

int* global_buffer;

static void allocate_global_buffer() {
  global_buffer = (int*)malloc(sizeof(int));
}

// the global is not written in the loop body itself, so it keeps the value
// freed before the loop was cut
void FP_use_after_free_of_global_reallocated_by_callee_in_loop_ok() {
  for (int i = 0; i < 10; i++) {
    if (i == 2) {
      free(global_buffer);
    }
    if (i == 5) {
      allocate_global_buffer();
    }
  }
  if (global_buffer != NULL) {
    *global_buffer = 42;
  }
}

int global_cell;

static int* pick(int* a, int* b) { return &global_cell; }

// writes through the pointer returned by a call with several pointer
// arguments are not traced back to a variable, so [global_cell] keeps the
// value it had when the loop was cut
void FP_write_through_returned_global_ok() {
  int x, y;
  for (int i = 0; i < 12; i++) {
    *pick(&x, &y) = i;
  }
  if (global_cell != 11) {
    int* p = NULL;
    *p = 42;
  }
}

// the counter is havoced, so the exit test of this infinite loop can succeed
void FP_npe_after_infinite_loop_assigning_counter_ok() {
  int i = 0;
  while (i < 10) {
    i = i * 1;
  }
  int* p = NULL;
  *p = 42;
}

// pointers only updated from their own value keep that value to stay attached
// to the data they traverse, so the exit of this loop stays out of reach
void FN_npe_after_pointer_increment_loop_bad() {
  int buf[8];
  for (int* p = buf; p < buf + 8; p++) {
    *p = 0;
  }
  int* q = NULL;
  *q = 42;
}

int global_cube[4][4][4];

void npe_after_triply_nested_constant_loops_bad() {
  for (int i = 0; i < 4; i++) {
    for (int j = 0; j < 4; j++) {
      for (int k = 0; k < 4; k++) {
        global_cube[i][j][k] = 0;
      }
    }
  }
  int* p = NULL;
  *p = 42;
}

// the disjuncts of the three branches fill the loop head up to the disjunct
// limit, which drops the havoced states
void FN_npe_after_loop_after_three_branches_bad(int a, int b, int c) {
  int x = 0;
  if (a) {
    x++;
  } else {
    x--;
  }
  if (b) {
    x++;
  } else {
    x--;
  }
  if (c) {
    x++;
  } else {
    x--;
  }
  for (int i = 0; i < 16; i++) {
    global_array[i] = x;
  }
  int* p = NULL;
  *p = 42;
}

// the paths through the body fill the loop head up to the disjunct limit,
// which drops the havoced states
void FN_npe_after_loop_with_branchy_body_bad(int* a) {
  for (int i = 0; i < 16; i++) {
    if (a[i] > 0) {
      global_array[i] = 1;
    } else if (a[i] < 0) {
      global_array[i] = 2;
    } else {
      global_array[i] = 3;
    }
  }
  int* p = NULL;
  *p = 42;
}

struct pointers {
  int* elements[16];
  int* other;
};

struct pointers global_pointer_struct;

static void fill_global_pointer_struct() {
  for (int i = 0; i < 16; i++) {
    global_pointer_struct.elements[i] = &global_target;
  }
}

// a struct or array global assigned in a cut loop is havoced as a whole, also
// in callers
void FN_npe_on_other_field_of_global_after_call_bad() {
  global_pointer_struct.other = NULL;
  fill_global_pointer_struct();
  *(global_pointer_struct.other) = 42;
}

// the loop body is not executed again from the havoced states
void FN_npe_in_late_iteration_bad() {
  int* p = NULL;
  for (int i = 0; i < 10; i++) {
    if (i == 7) {
      *p = 42;
    }
  }
}

// the havoced states are dropped at the first branch inside the loop, so the
// exit test behind it is not reached
void FN_npe_after_do_while_loop_with_if_bad() {
  int i = 0;
  do {
    if (i % 2) {
      global_array[i] = 0;
    }
    i++;
  } while (i < 16);
  int* p = NULL;
  *p = 42;
}

// havocing the memory written through [buf] also forgets that it is allocated
void FN_leak_of_buffer_filled_in_loop_bad() {
  int* buf = (int*)malloc(16 * sizeof(int));
  if (buf == NULL) {
    return;
  }
  for (int i = 0; i < 16; i++) {
    buf[i] = 0;
  }
}
