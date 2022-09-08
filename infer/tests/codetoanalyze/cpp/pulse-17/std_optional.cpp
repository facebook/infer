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
