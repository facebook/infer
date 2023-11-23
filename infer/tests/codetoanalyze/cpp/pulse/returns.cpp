/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <map>
#include <string>

namespace returns {

struct S {
  int f_;

  S(int f) : f_(f) {}

  ~S() {}
};

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreturn-stack-address"
const int& return_literal_stack_reference_bad() { return 1; }

const int& return_variable_stack_reference1_bad() {
  const int& x = 2;
  return x;
}

const int& return_variable_stack_reference2_bad() {
  const int& x = 2;
  const int& y = x;
  return y;
}
#pragma clang diagnostic pop

const int return_read_of_stack_reference_ok() {
  const int& x = 2;
  return x;
}

const int& return_formal_reference_ok(int& formal) { return formal; }

const int& return_reference_to_formal_pointer_ok(const int* formal) {
  const int& local = *formal;
  return local;
}

extern const int& callee();

const int& return_reference_from_callee_ok() {
  const int& local = callee();
  return local;
}

const int return_int_ok() { return 1; }

const bool return_comparison_temp_ok() { return 1 != 2; }

const bool compare_local_refs_ok() {
  const int& local1 = 1;
  const int& local2 = 1;
  return local1 != local2;
}

extern int& global;

const int& return_global_reference_ok() { return global; }

struct MemberReference {
  int& member1;

  int& return_member_reference_ok() { return member1; }

  int* member2;
  int* return_member_reference_indirect_ok() {
    int* local = member2;
    return local;
  }
};

extern const char* const kOptions;

const char* return_field_addr_ternary_ok() {
  const char* const* const t = &kOptions;
  return t ? *t : "";
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreturn-stack-address"
int* return_stack_pointer_bad() {
  int x = 3;
  return &x;
}
#pragma clang diagnostic pop

S* return_static_local_ok() {
  S* local;
  static S s{1};
  local = &s;
  return local;
}

static const S& return_static_local_ref_ok() {
  static const S& s{1};
  return s;
}

S* return_static_local_inner_scope_ok(bool b) {
  S* local = nullptr;
  if (b) {
    static S s{1};
    local = &s;
    // destructor for s gets called here, but it shouldn't be
  }
  return local;
}

int* return_formal_pointer_ok(int* formal) { return formal; }

// this *could* be ok depending on what the caller does
int* return_deleted_ok() {
  int* x = new int;
  *x = 2;
  delete x;
  return x;
}

// this *could* be ok depending on what the caller does
S* return_destructed_pointer_ok() {
  S* s = new S(1);
  s->~S();
  return s;
}

const char* return_nullptr1_ok() { return nullptr; }

const char* return_nullptr2_ok() {
  const char* local = nullptr;
  return local;
}

struct A {
  ~A();
};

int try_catch_return_ok() {
  A a;
  try {
    return 1;
  } catch (...) {
    return 2;
  }
}

std::map<int, std::string> global_map;

const int& return_structured_variable_ok() {
  for (const auto& [key, _] : global_map) {
    return key;
  }
}
} // namespace returns
