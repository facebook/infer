/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <optional>
#include <set>
#include <string>
#include <utility>
#include <vector>
#include <functional>

namespace folly {
template <class Value>
class Optional {
 public:
  Optional(const Optional& src);
  bool has_value();
};

namespace coro {
template <class Value>
class Task {};
} // namespace coro
} // namespace folly

namespace const_refable {

struct Arr {
  int arr[2];
  std::vector<int> vec;
};

// The correct use of const reference when parameter is not modified
int read_first_ok(const Arr& a) { return a.vec[0]; }

// Non-const references are not reported on
std::string non_const_ref_ok(std::string& str) { return str + str; }

// R-value references are not reported on
void rvalue_ref_ok(Arr&& a) { auto& cpy = a; }

// vec is not modified, so should be const reference.
int read_first_bad(Arr a) { return a.vec[0]; }

int modify_first_ok(Arr a) {
  a.arr[0] += 8;
  return a.arr[0];
}

void ref_modified_ok(std::vector<int> vec) {
  auto& cpy = vec;
  cpy[0] += 8;
}

int get_first(const std::vector<int>& vec) { return vec[0]; }

void interprocedural_read_bad(std::vector<int> vec) {
  int first = get_first(vec);
}

// Sum vec by reading vector elements
int sum_in_loop_bad(std::vector<int> vec) {
  int total = 0;
  for (const int& x : vec)

    total += x; // vec is not modified here
  return total;
}

// anonymous parameters usually exist to satisfy the signature for virtual
// functions. If we recommend const ref, the signature would not match and
// making the base function const-refable might not be possible since it might
// be using the parameter.
void some_fun_ok(std::vector<int> /*unnamed_param*/) {}

void pass_rvalue_ref(std::set<int>&& x) {}

// params which are passed-by-value and get moved are usually intentional
int move_ok(std::set<int> source) {
  pass_rvalue_ref(std::move(source));
  return 0;
}

int param_ref_move_ok(std::set<int> source) {
  auto& source_ref = source;
  pass_rvalue_ref(std::move(source_ref));
  return 0;
}

// structs known to be cheap to copy are not reported
int std_pair_int_ok(std::pair<int, int> p) { return p.first; }

int std_pair_vector_bad(std::pair<int, std::vector<int>> p) { return p.first; }

std::string std_pair_string_bad(std::pair<std::string, std::string> p) {
  return p.first;
}

int folly_optional_int_ok(folly::Optional<int> n_opt) {
  if (n_opt.has_value()) {
    return 42;
  }
  return 0;
}

int folly_optional_vector_bad(folly::Optional<std::vector<int>> vec_opt) {
  if (vec_opt.has_value()) {
    return 42;
  }
  return 0;
}

int folly_optional_string_bad(folly::Optional<std::string> s_opt) {
  if (s_opt.has_value()) {
    return 42;
  }
  return 0;
}

struct StructWithInt {
  int n;
  std::vector<int> vec;
};

void havoc_ptr(int* p);

// StructWithInt s is const refable since it is not modified
void havoc_reachable_by_unknown_bad(int* p, StructWithInt s) {
  if (s.n == 42 && *p == 42) {
    havoc_ptr(p);
  }
}

void dead_param_ok(folly::Optional<std::string> s_opt) {}

void call_lambda(const std::function<void()>& f) { f(); }

// This is TP in c++11.
void captured_arr_bad_FN(Arr a) {
  call_lambda([&a]() {});
}

// This is FP in c++11.
void captured_arr_ok(Arr a) {
  call_lambda([&a]() { a.arr[0] += 8; });
}

// This is TP in c++11.
void captured_shared_ptr_bad_FN(std::shared_ptr<int> a) {
  call_lambda([&a]() {});
}

Arr global;

class AssignField {
  Arr field;

 public:
  // It should NOT report const refable issue, but unncessary copy assignment
  // issue.
  void assign_field_bad_FN(Arr a) { field = a; }

  // It should NOT report const refable issue, but unncessary copy assignment
  // issue.
  void assign_global_bad(Arr a) { global = a; }
};

// Suppress const refable issues on functions returning folly::coro::Task
folly::coro::Task<Arr> ret_coro_task_ok(std::string s) {
  int n = s.length();
  return folly::coro::Task<Arr>();
}

} // namespace const_refable
