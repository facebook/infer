/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <exception>
#include <mutex>
#include <new>
#include <stdexcept>
#include <thread>

namespace infer {
class ScopeGuard {};
}; // namespace infer

namespace dead_stores {

void easy_bad() { int x = 5; }

void reassign_param_bad(int x) { x = 5; }

int dead_then_live_bad() {
  int x = 5;
  x = 3;
  return x;
}

int use_then_dead_bad() {
  int x = 5;
  int y = x;
  x = 7;
  return y;
}

void dead_pointer_bad() {
  int num = 2;
  int* x = &num;
}

void plus_plus1_bad() {
  int i = 1;
  ++i;
}

void plus_plus2_bad() {
  int i = 1;
  i++;
}

int plus_plus3_bad() {
  int i = 1;
  return i++;
}

void FN_capture_no_read_bad() {
  int x = 1;
  [x]() { return; }();
}

void init_capture_reassign_bad() {
  int i = 1; // this is a dead store
  return [i = 1]() { return i; }();
}

void init_capture_no_call_bad() {
  [i = 1]() { return i; };
}

int FN_init_capture_no_read_bad() {
  return [i = 1]() { return 0; }();
}

int return_ok() {
  int x = 5;
  return x;
}

int branch_ok(bool b) {
  int x = 5;
  int y = 3;
  if (b) {
    y = x;
  }
  return y;
}

int loop_ok(bool b) {
  int x = 5;
  int y = 3;
  while (b) {
    y = x;
    b = false;
  }
  return y;
}

int loop_break_ok(bool b) {
  int x = 5;
  while (b) {
    x = 3;
    break;
  }
  return x;
}

int loop_continue_ok(bool b) {
  int x = 5;
  int y = 2;
  while (b) {
    y = x;
    x = 3;
    continue;
  }
  return y;
}

void assign_pointer1_ok(int* ptr) { *ptr = 7; }

int* assign_pointer2_ok() {
  int num = 2;
  int* ptr = &num;
  return ptr;
}

void by_ref1_ok(int& ref) { ref = 7; }

void by_ref2_ok(int& ref) { ref++; }

int plus_plus_ok() {
  int x = 1;
  return ++x;
}

int plus_plus_loop_ok(int n) {
  int i;
  for (i = 1; i < n; i++) {
    i++;
  }
  return i;
}

void lambda_bad() {
  int x = []() {
    int y = 1;
    y = 2;
    return y;
  }();
  return x;
}

void capture1_ok() {
  int x = 1;
  [x]() { return x; }();
}

void capture2_ok(int x) {
  [x]() { return x; }();
}

int capture_by_ref1_ok() {
  int x = 1;
  [&x]() { x++; }();
  return x;
}

int capture_by_ref2_ok() {
  int x = 1;
  int y = 1;
  [&]() {
    x = x + y;
    y = x;
  }();
  return x + y;
}

int capture_by_ref3_ok() {
  int x = 1;
  [&](auto y) { x += y; }(3);
  return x;
}

int capture_by_ref4_ok() {
  int x = 1;
  auto lambda = [&] { return x; };
  x = 2; // not a dead store; updates captured x
  return lambda();
}

int dead_store_before_capture_by_ref_bad() {
  int x = 1; // this is dead. should report it even though x is captured by ref
             // later on
  x = 2;
  auto lambda = [&] { return x; };
  x = 2;
  return lambda();
}

int capture_by_value_bad() {
  int x = 1;
  auto lambda = [=] { return x; };
  x = 2; // this is dead
  return lambda();
}

int FN_capture_by_ref_reuseBad() {
  int x = 1;
  [&x]() {
    x = 1; // dead, but we won't report
    x = 2;
  }();
  return x;
}

int init_capture1_ok() {
  return [i = 1]() { return i; }();
}

int init_capture2_ok() {
  int i = 1;
  return [j = i]() { return j; }();
}

int init_capture3_ok() {
  int i = 1;
  return [i = i]() { return i; }();
}

int init_capture4_ok() {
  int i = 1;
  int j = 1;
  return [a = 1, b = i, c = j]() { return a + b + c; }();
}

int init_capture5_ok() {
  int i = 1;
  int k = [j = i]() { return j; }();
  i = 5; // should not be flagged
  return i + k;
}

int init_capture6_ok() {
  int i = 1;
  int k = [i = i + 1]() { return i; }();
  i = 5; // should not be flagged;
  return i + k;
}

char* global;

void assign_array_tricky_ok() {
  char arr[1];
  global = arr;
  *(int*)arr = 123;
}

// Currently the frontend does not translate the casting of pointers to float.
void FP_assign_array_tricky2_ok() {
  char arr[1];
  global = arr;
  *(float*)arr = 1.0;
}

void placement_new_ok(int len, int* ptr) {
  int* placement = ptr;
  while (len--) {
    new (placement++) int(5);
  }
}

// we don't report on dead stores where the RHS is 0, 0.0, false, nullptr, etc.
bool sentinel_bool_ok() {
  bool b = false;
  b = true;
  return b;
}

int sentinel_int_ok() {
  int i = 0;
  i = 1;
  return i;
}

int sentinel_long_ok() {
  long l = 0L;
  l = 1L;
  return l;
}

float sentinel_float_ok() {
  float f = 0.0;
  f = 1.0;
  return f;
}

double sentinel_double_ok() {
  double d = 0.0;
  d = 1.0;
  return d;
}

int* sentinel_ptr_ok(int* j) {
  int* i = nullptr;
  i = j;
  return i;
}

void custom_scope_guard_ok() { infer::ScopeGuard guard; }

struct S {
  ~S() {}
};

typedef S&& B;

S mk_s() {
  S s;
  return s;
};

// s gets read by the destructor for S
void FN_dead_struct_value1_bad() { S s = mk_s(); }

// need to handle operator= in order to detect this case
void FN_dead_struct_value2_bad() {
  S s = mk_s();
  s = mk_s();
}

void dead_struct_rvalue_ref_bad() { B b = mk_s(); }

S struct_value_used_ok() {
  S s = mk_s();
  return s;
}

B& struct_rvalue_ref_used_ok() {
  B b = mk_s();
  return b;
}

struct NoDestructor {};

void dead_struct_no_destructor_bad() { NoDestructor dead; }

void no_destructor_void_read_ok() {
  NoDestructor dead;
  (void)dead;
}

struct NoDestructorDefinition {
  ~NoDestructorDefinition();
};

void dead_struct_no_destructor_definition_ok() { NoDestructorDefinition dead; }

std::mutex my_mutex;

void dead_lock_guard_ok() { std::lock_guard<std::mutex> lock(my_mutex); }

void dead_unique_lock_ok() { std::unique_lock<std::mutex> lock(my_mutex); }

extern int maybe_throw();

class Exceptions {

  int read_in_catch1_ok() {
    int i = 1;
    try {
      throw std::runtime_error("error");
    } catch (...) {
      return i;
    }
    return 0;
  }

  int read_in_catch_explicit_throw_ok() {
    int i = 1;
    try {
      maybe_throw();
    } catch (...) {
      return i;
    }
    return 0;
  }

  int dead_in_catch_bad() {
    try {
      throw std::runtime_error("error");
    } catch (...) {
      int i = 1;
    }
    return 0;
  }

  int unreachable_catch_bad() {
    int i = 1;
    try {
    } catch (...) {
      return i;
    }
    return 0;
  }

  int multiple_catches_ok(bool b) {
    int i = 1;
    int j = 2;
    try {
      if (b) {
        throw std::length_error("error");
      } else {
        throw std::range_error("error");
      }
    } catch (std::length_error& msg) {
      return i;
    } catch (std::range_error& msg) {
      return j;
    }
    return 0;
  }

  void dont_throw() {}

  int FN_harder_unreachable_catch_bad() {
    int i = 1;
    try {
      dont_throw();
    } catch (...) {
      return i;
    }
    return 0;
  }

  // currently, the only transition to the catch block is at the end of the try
  // block
  int FP_read_in_catch_tricky_ok(bool b1, bool b2) {
    int i = 1;
    try {
      if (b1) {
        throw std::runtime_error("error");
      }
      i = 2;
      if (b2) {
        throw std::runtime_error("error");
      }
    } catch (...) {
      return i;
    }
    return 0;
  }

  int return_in_try1_ok() {
    bool b;

    try {
      maybe_throw();
      return 3;
    } catch (const char* msg) {
      b = true;
    }

    if (b) {
      return 2;
    }
    return 3;
  }

  int return_in_try2_ok() {
    bool b;

    try {
      return maybe_throw();
    } catch (const char* msg) {
      b = true;
    }

    if (b) {
      return 2;
    }
    return 3;
  }
};

void init_in_binop_bad(int x) { x = -x & ~int{0}; }

void unused_tmp_bad() { int __tmp = 1; }

#define UNUSED(a) __typeof__(&a) __attribute__((unused)) __tmp = &a;

void unused_attribute_tmp_ok() {
  int x;
  UNUSED(x);
}

void unused_attribute_ok() { int __attribute__((unused)) x = 42; }

struct ChainedCalls {
  ChainedCalls chained(int i);
};

ChainedCalls chain_method_calls_ok() {
  ChainedCalls x;
  return x.chained(5).chained(6);
}

struct A {
  int f : 4;
};

int decltype_read_ok(int x) {
  A a;
  decltype(a.f) i;
  return x + i;
}

// destructor blacklisted for liveness in .inferconfig
struct BlacklistedStruct {
  ~BlacklistedStruct(){};

  BlacklistedStruct cloneAsValue() const { return BlacklistedStruct(); }

  std::unique_ptr<BlacklistedStruct> clone() const {
    return std::make_unique<BlacklistedStruct>(cloneAsValue());
  }
};

void unused_blacklisted_constructed_bad() { auto x = BlacklistedStruct(); }

void unused_blacklisted_clone_bad(BlacklistedStruct* something) {
  auto x = something->clone();
}

void unused_blacklisted_unique_ptr_bad(BlacklistedStruct* something) {
  auto x = std::make_unique<BlacklistedStruct>(*something);
}

void unused_unique_ptr_good(A* something) {
  auto x = std::make_unique<A>(*something);
}

} // namespace dead_stores
