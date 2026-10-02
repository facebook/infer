/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <optional>
#include <vector>
#include <memory>
#include <string>

int std_not_none_ok() {
  std::optional<int> foo{5};
  return foo.value();
}

int std_not_none_check_value_ok() {
  std::optional<int> foo{5};
  int x = foo.value();
  if (x != 5) {
    std::optional<int> foo{std::nullopt};
    return foo.value();
  }
  return x;
}

int std_none_check_ok() {
  std::optional<int> foo{std::nullopt};
  if (foo) {
    return foo.value();
  }
  return -1;
}

int std_none_check_has_value_ok() {
  std::optional<int> foo{std::nullopt};
  if (foo.has_value()) {
    return foo.value();
  }
  return -1;
}

int std_none_no_check_bad() {
  std::optional<int> foo{std::nullopt};
  return foo.value();
}

int std_none_copy_ok() {
  std::optional<int> foo{5};
  std::optional<int> bar{foo};
  return bar.value();
}

int std_none_copy_bad() {
  std::optional<int> foo{std::nullopt};
  std::optional<int> bar{foo};
  return bar.value();
}

int std_assign_ok() {
  std::optional<int> foo{5};
  std::optional<int> bar{foo};
  foo = std::nullopt;
  return bar.value();
}

int std_assign_bad() {
  std::optional<int> foo{std::nullopt};
  std::optional<int> bar{5};
  int sum = bar.value();
  bar = foo;
  sum += bar.value();
  return sum;
}

int std_assign2_bad() {
  std::optional<int> foo{5};
  int sum = foo.value();
  foo = std::nullopt;
  sum += foo.value();
  return sum;
}

struct State {
  std::vector<int> vec;
};

void std_emplace(std::optional<State> state) {
  if (state) {
    state.emplace();
  }
  auto pos = state->vec.begin();
}

void std_operator_arrow_bad() { std_emplace(std::nullopt); }

int std_value_or_check_empty_ok() {
  std::optional<int> foo{std::nullopt};
  if (foo.value_or(0) > 0) {
    return foo.value();
  }
  return -1;
}

int std_value_or_check_value_ok() {
  std::optional<int> foo{5};
  int x = foo.value_or(0);
  if (x != 5) {
    std::optional<int> foo{std::nullopt};
    return foo.value();
  }
  return -1;
}
std::optional<std::string> might_return_none(bool b, std::string x) {
  if (b) {
    return std::nullopt;
  }
  return x;
}

std::string reassing_non_empty_ok(const std::string& x) {
  std::optional<std::string> foo = might_return_none(true, x);
  if (!foo.has_value()) {
    foo = x;
  }

  return foo.value();
}

enum E { OP1, OP2 };

constexpr const char* envVar = "ENV_VAR";

E getEnum() {
  auto value = std::getenv(envVar);
  if (value) {
    return E::OP1;
  }

  return E::OP2;
}

std::optional<std::string> getOptionalValue() {
  auto value = std::getenv(envVar);
  if (value) {
    return std::string{value};
  }
  return std::nullopt;
}

std::optional<std::string> FP_cannot_be_empty() {
  if (getEnum() == E::OP1) {
    return getOptionalValue().value();
  }
  return std::nullopt;
}

std::string inside_try_catch_FP(const std::string& x) {
  std::optional<std::string> foo = might_return_none(true, x);
  try {
    return foo.value();
  } catch (...) {
    return "";
  }
}

int unknown_pure();

std::optional<std::string> get_value_pure() {
  if (unknown_pure()) {
    return std::optional<std::string>{"Godzilla"};
  }
  return std::nullopt;
}

std::string call_get_value_pure_twice_ok() {
  if (get_value_pure().has_value()) {
    return *get_value_pure();
  }
  return "";
}

// an empty optional must not be confused with null pointers on the same path

int std_null_deref_after_nullopt_bad() {
  std::optional<int> foo;
  int* p = nullptr;
  return *p;
}

int std_copy_shared_ptr_after_nullopt_ok(const std::shared_ptr<int>& sp,
                                         std::optional<int>& foo) {
  foo = std::nullopt;
  std::shared_ptr<int> copy = sp;
  return *copy;
}

int std_destroy_empty_ok() {
  std::optional<std::string> foo;
  return 0;
}

std::optional<int> unknown_optional();

void fill_optional(std::optional<int>& foo);

int lazy_fill(std::optional<int>& foo) {
  if (!foo) {
    fill_optional(foo);
  }
  return *foo;
}

int std_lazy_fill_empty_ok() {
  std::optional<int> foo;
  return lazy_fill(foo);
}

// accessing the value shows that the optional is not empty on that path

int std_deref_then_copy_ok() {
  std::optional<int> foo = unknown_optional();
  int x = *foo;
  std::optional<int> bar = foo;
  return x + *bar;
}

int std_deref_then_check_ok() {
  std::optional<int> foo = unknown_optional();
  int x = *foo;
  if (!foo.has_value()) {
    int* p = nullptr;
    return *p;
  }
  return x;
}

struct OptionalHolder {
  std::optional<int> value;
  void set(const std::optional<int>& foo) { value = foo; }
};

int deref_then_set(OptionalHolder& holder, const std::optional<int>& foo) {
  int x = *foo;
  holder.set(foo);
  return x + *foo;
}

int std_deref_then_set_empty_bad(OptionalHolder& holder) {
  std::optional<int> foo;
  return deref_then_set(holder, foo);
}

int std_deref_then_set_unknown_ok(OptionalHolder& holder) {
  std::optional<int> foo = unknown_optional();
  return deref_then_set(holder, foo);
}

int* get_pointer_or_null(std::optional<int>& foo) {
  return foo ? &*foo : nullptr;
}

int deref_then_get_pointer(std::optional<int>& foo) {
  int x = *foo;
  return x + *get_pointer_or_null(foo);
}

int deref_then_get_pointer_unknown_ok() {
  std::optional<int> foo = unknown_optional();
  return deref_then_get_pointer(foo);
}

const char* c_str_or_null(const std::optional<std::string>& foo) {
  return foo ? foo->c_str() : nullptr;
}

std::size_t size_then_c_str(const std::optional<std::string>& foo) {
  std::size_t n = foo->size();
  return n + c_str_or_null(foo)[0];
}

std::optional<std::string> unknown_optional_string();

std::size_t size_then_c_str_unknown_ok() {
  std::optional<std::string> foo = unknown_optional_string();
  return size_then_c_str(foo);
}

std::size_t std_size_then_c_str_empty_bad() {
  std::optional<std::string> foo;
  return size_then_c_str(foo);
}

const int* value_pointer_or_null(const std::optional<int>& foo) {
  return foo ? &foo.value() : nullptr;
}

int value_then_value_pointer(const std::optional<int>& foo) {
  int x = foo.value();
  return x + *value_pointer_or_null(foo);
}

int value_then_value_pointer_unknown_ok() {
  std::optional<int> foo = unknown_optional();
  return value_then_value_pointer(foo);
}

int access(const std::optional<int>& foo) { return *foo; }

int std_access_in_callee_then_check_ok() {
  std::optional<int> foo = unknown_optional();
  int x = access(foo);
  if (!foo) {
    int* p = nullptr;
    return *p;
  }
  return x;
}

struct OptionalMember {
  std::optional<int> value;

  int access_after_branches_bad(bool b0, bool b1, bool b2, bool b3, bool b4) {
    int x = 0;
    if (b0) {
      x += 1;
    }
    if (b1) {
      x += 2;
    }
    if (b2) {
      x += 4;
    }
    if (b3) {
      x += 8;
    }
    if (b4) {
      x += 16;
    }
    int y = *value;
    if (x == 30) {
      int* p = nullptr;
      return *p + y;
    }
    return y;
  }
};

// testing an optional in a callee must not make unrelated issues latent

struct OptionalConfig {
  std::optional<int> timeout;
  int get_timeout() const { return timeout ? *timeout : 30; }
  std::optional<int> get() const { return timeout; }
};

int std_null_deref_after_checking_getter_bad(const OptionalConfig& config) {
  int t = config.get_timeout();
  int* p = nullptr;
  return t + *p;
}

int std_null_deref_after_copying_getter_bad(const OptionalConfig& config) {
  std::optional<int> t = config.get();
  int* p = nullptr;
  return *p;
}

int std_null_deref_if_both_empty_bad(const std::optional<int>& foo,
                                     const std::optional<int>& bar) {
  if (!foo && !bar) {
    int* p = nullptr;
    return *p;
  }
  return 0;
}
