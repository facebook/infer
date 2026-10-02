/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// test templated globals manipulations

template <typename T>
bool templated_global;

void set_templated_global_int(bool b) { templated_global<int> = b; }

void set_templated_global_bool(bool b) { templated_global<bool> = b; }

// check that we handle different instantiations of the same templated global
// correctly
void several_instantiations_bad() {
  set_templated_global_int(true);
  set_templated_global_bool(false);
  if (templated_global<int> && !templated_global<bool>) {
    int* p = nullptr;
    *p = 42;
  }
}

// test global initializers

constexpr bool yes = true;

template <typename T>
constexpr bool templated_const_global = false;

template <>
constexpr bool templated_const_global<int> = true;

struct X;

void read_templated_const_global_then_crash_bad() {
  if (yes && templated_const_global<int> && !templated_const_global<X>) {
    int* p = nullptr;
    *p = 42;
  }
}

// calls through function pointers stored in global constants

struct Obj {
  int val;
};

struct ObjOps {
  void (*release)(Obj*);
};

static void delete_obj(Obj* o) { delete o; }

static const ObjOps kDeleteOps{&delete_obj};

constexpr ObjOps kConstexprDeleteOps{&delete_obj};

int call_const_global_field_bad() {
  Obj* o = new Obj{1};
  kDeleteOps.release(o);
  return o->val;
}

int call_constexpr_global_field_bad() {
  Obj* o = new Obj{1};
  kConstexprDeleteOps.release(o);
  return o->val;
}

// the initializer of a global constant is inlined at most once per path

struct AllocatesInConstructor {
  int* p;
  AllocatesInConstructor() : p(new int(1)) {}
  int get() const { return *p; }
};

static const AllocatesInConstructor kAllocates;

int read_const_global_field_twice_ok() { return *kAllocates.p + *kAllocates.p; }

int call_const_global_method_twice_ok() {
  return kAllocates.get() + kAllocates.get();
}

int read_const_global_field_in_callee() { return *kAllocates.p; }

int read_const_global_field_in_callee_and_caller_ok() {
  int x = read_const_global_field_in_callee();
  return x + *kAllocates.p;
}

// the summary of the callee inlines the initializer of [kAllocates] again,
// which overwrites the pointer allocated when the caller inlined it
int FP_read_const_global_field_in_caller_and_callee_ok() {
  int x = *kAllocates.p;
  return x + read_const_global_field_in_callee();
}

struct WithMutableField {
  mutable int cache;
  int val;
};

static const WithMutableField kWithMutableField{0, 1};

void write_mutable_field_of_const_global_ok() {
  kWithMutableField.cache = 1;
  if (kWithMutableField.cache != 1) {
    int* p = nullptr;
    *p = 42;
  }
}

int read_field_of_const_global_with_mutable_field() {
  return kWithMutableField.val;
}

// the summary of the callee inlines the initializer of [kWithMutableField]
// again, which resets [cache]
void FP_write_mutable_field_of_const_global_then_call_ok() {
  kWithMutableField.cache = 1;
  read_field_of_const_global_with_mutable_field();
  if (kWithMutableField.cache != 1) {
    int* p = nullptr;
    *p = 42;
  }
}

// global constants whose initializers refer to each other

struct Node {
  const Node* next;
  int val;
  Node(const Node& next, int val) : next(&next), val(val) {}
};

extern const Node kNodeA;

const Node kNodeB(kNodeA, 2);

const Node kNodeA(kNodeB, 1);

void read_cyclic_const_globals_ok() {
  if (kNodeB.next != &kNodeA || kNodeA.next != &kNodeB) {
    int* p = nullptr;
    *p = 42;
  }
}

// the initializers of constants that read non-const globals are not inlined:
// the values they compute depend on when they run

int g_mode = 1;

struct Config {
  int doubled;
};

const Config kDynamicConfig{g_mode * 2};

void dynamic_init_const_field_bad() {
  g_mode = 5;
  if (kDynamicConfig.doubled == 2) {
    int* p = nullptr;
    *p = 42;
  }
}

const int kDynamicInt = g_mode * 2;

void dynamic_init_const_global_bad() {
  g_mode = 5;
  if (kDynamicInt == 2) {
    int* p = nullptr;
    *p = 42;
  }
}

static int get_mode() { return g_mode; }

struct ScaledOps {
  void (*release)(Obj*);
  int scale;
};

const ScaledOps kDynamicOps{&delete_obj, get_mode() * 3};

void dynamic_init_const_ops_bad() {
  g_mode = 7;
  if (kDynamicOps.scale == 3) {
    int* p = nullptr;
    *p = 42;
  }
}

// the whole initializer is skipped, including the fields that do not depend on
// other globals
void FN_call_dynamic_init_const_field_bad() {
  Obj* o = new Obj{1};
  kDynamicOps.release(o);
  delete o;
}
