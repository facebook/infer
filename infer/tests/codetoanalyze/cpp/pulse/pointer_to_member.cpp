/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

// Pointers to members are not modelled: accesses and calls through them are
// unknown calls that may modify the object.

namespace pointer_to_member {

struct Pair {
  int* first;
  int* second;
};

void read_data_member_then_npe_bad(Pair& pair, int* Pair::*field) {
  int* value = pair.*field;
  (void)value;
  int* p = nullptr;
  *p = 42;
}

void write_data_member_then_npe_bad(Pair* pair, int* Pair::*field) {
  pair->*field = nullptr;
  int* p = nullptr;
  *p = 42;
}

void address_of_data_member_then_npe_bad() {
  int* Pair::*field = &Pair::first;
  (void)field;
  int* p = nullptr;
  *p = 42;
}

int deref_null_object_data_member_bad(int* Pair::*field) {
  Pair* pair = nullptr;
  return *(pair->*field);
}

void write_data_member_of_uninitialized_ok(int* Pair::*field) {
  Pair pair;
  pair.*field = nullptr;
}

static void set_field(Pair& pair, int* Pair::*field, int* value) {
  pair.*field = value;
}

int write_data_member_in_callee_ok() {
  Pair pair{nullptr, nullptr};
  int x = 42;
  set_field(pair, &Pair::first, &x);
  return *pair.first;
}

static void set_field_then_branch(Pair& pair, int* Pair::*field, int c) {
  pair.*field = nullptr;
  if (c)
    (void)c;
}

void call_set_field_then_branch_then_npe_bad(Pair& pair, int* Pair::*field) {
  set_field_then_branch(pair, field, 1);
  int* p = nullptr;
  *p = 42;
}

void write_conditional_data_member_then_npe_bad(Pair& pair,
                                                int* Pair::*field1,
                                                int* Pair::*field2,
                                                int c) {
  pair.*(c ? field1 : field2) = nullptr;
  int* p = nullptr;
  *p = 42;
}

void write_data_member_after_label_then_npe_bad(Pair& pair,
                                                int* Pair::*field) {
  goto write;
write:
  pair.*field = nullptr;
  int* p = nullptr;
  *p = 42;
}

// a read through a pointer to member is an unknown call that may modify the
// object, so it forgets the values of all the members
int FN_read_data_member_keeps_other_member_bad(Pair& pair, int* Pair::*field) {
  pair.first = nullptr;
  (void)(pair.*field);
  return *pair.first;
}

struct Counters {
  int hits;
  int misses;
  int get(int Counters::*counter) const { return this->*counter; }
};

// the read through [counter] forgets the value written through it
void FP_write_then_read_same_data_member_ok(Counters& counters,
                                            int Counters::*counter) {
  counters.*counter = 7;
  if (counters.*counter != 7) {
    int* p = nullptr;
    *p = 42;
  }
}

// the read through [counter] forgets the value of [hits]
void FP_read_data_member_then_check_other_ok(Counters& counters,
                                             int Counters::*counter) {
  counters.hits = 1;
  (void)(counters.*counter);
  if (counters.hits != 1) {
    int* p = nullptr;
    *p = 42;
  }
}

// same, although the object is const
void FP_read_const_data_member_then_check_other_ok(const Counters& counters,
                                                   int Counters::*counter) {
  int hits = counters.hits;
  (void)(counters.*counter);
  if (counters.hits != hits) {
    int* p = nullptr;
    *p = 42;
  }
}

// same, in a callee that is a const method
void FP_call_const_getter_then_check_other_ok(Counters& counters,
                                              int Counters::*counter) {
  counters.hits = 1;
  (void)counters.get(counter);
  if (counters.hits != 1) {
    int* p = nullptr;
    *p = 42;
  }
}

// pointers to members are not modelled, so [counters.*counter] is not known to
// be [counters.hits]
void FP_write_through_known_ptr_to_member_then_read_ok(Counters& counters) {
  int Counters::*counter = &Counters::hits;
  counters.*counter = 5;
  if (counters.hits != 5) {
    int* p = nullptr;
    *p = 42;
  }
}

// the value of [&Counters::hits] is not modelled, so it may be null
void FP_check_ptr_to_data_member_not_null_ok() {
  int Counters::*counter = &Counters::hits;
  if (!counter) {
    int* p = nullptr;
    *p = 42;
  }
}

struct Buffers {
  int* data;
  int* spare;
};

void store_malloc_ok(Buffers& buffers, int* Buffers::*field) {
  buffers.*field = (int*)malloc(sizeof(int));
}

void store_malloc_arrow_ok(Buffers* buffers, int* Buffers::*field) {
  (buffers->*field) = (int*)malloc(sizeof(int));
}

void store_malloc_then_free_ok(Buffers& buffers, int* Buffers::*field) {
  buffers.*field = (int*)malloc(sizeof(int));
  free(buffers.*field);
}

static void alloc_into(int** p) { *p = (int*)malloc(sizeof(int)); }

void alloc_into_data_member_ok(Buffers& buffers, int* Buffers::*field) {
  alloc_into(&(buffers.*field));
}

static void release(Buffers* buffers) {
  free(buffers->data);
  free(buffers->spare);
}

void store_malloc_then_release_ok(int* Buffers::*field) {
  Buffers buffers{nullptr, nullptr};
  buffers.*field = (int*)malloc(sizeof(int));
  release(&buffers);
}

// the memory is stored into an unknown member of [buffers], which is not one
// of the members that [release] frees
void FP_alloc_into_data_member_then_release_ok(int* Buffers::*field) {
  Buffers buffers{nullptr, nullptr};
  alloc_into(&(buffers.*field));
  release(&buffers);
}

// a value assigned through a pointer to member escapes to an unknown call
void FN_store_malloc_in_local_bad(int* Buffers::*field) {
  Buffers buffers{nullptr, nullptr};
  buffers.*field = (int*)malloc(sizeof(int));
}

class Machine {
 public:
  using Handler = void (Machine::*)();

  void init() { state_ = &value_; }

  void step_then_npe_bad() {
    (this->*handler_)();
    int* p = nullptr;
    *p = 42;
  }

  int step_modifies_this_ok() {
    state_ = nullptr;
    (this->*handler_)();
    return *state_;
  }

 private:
  Handler handler_ = &Machine::init;
  int* state_ = nullptr;
  int value_ = 0;
};

struct Callbacks {
  void set_null(int** p) { *p = nullptr; }
  void set_to(int** p, int* v) { *p = v; }
};

void call_member_fn_ptr_then_npe_bad(Callbacks& c,
                                     void (Callbacks::*f)(int**)) {
  int* q;
  (c.*f)(&q);
  int* p = nullptr;
  *p = 42;
}

int call_member_fn_ptr_modifies_arg_ok(Callbacks& c) {
  void (Callbacks::*f)(int**, int*) = &Callbacks::set_to;
  int x = 0;
  int* q = nullptr;
  (c.*f)(&q, &x);
  return *q;
}

void call_conditional_member_fn_ptr_then_npe_bad(Callbacks& c,
                                                 void (Callbacks::*f)(int**),
                                                 void (Callbacks::*g)(int**),
                                                 int b) {
  int* q;
  (c.*(b ? f : g))(&q);
  int* p = nullptr;
  *p = 42;
}

void call_member_fn_ptr_null_object_bad(void (Callbacks::*f)(int**)) {
  Callbacks* c = nullptr;
  int* q;
  (c->*f)(&q);
}

// calls through pointers to member functions are not resolved to the member
// function that is called
int FN_call_member_fn_ptr_set_null_bad(Callbacks& c) {
  void (Callbacks::*f)(int**) = &Callbacks::set_null;
  int x = 0;
  int* q = &x;
  (c.*f)(&q);
  return *q;
}

} // namespace pointer_to_member
