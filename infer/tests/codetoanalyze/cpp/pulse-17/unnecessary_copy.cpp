/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <filesystem>
#include <optional>
#include <string>
#include <vector>
#include <map>

namespace unnecessary_copy {
struct Arr {
  int arr[2];
  std::vector<int> vec;
};

void get_optional_const_ref(const std::optional<Arr>& x) {}

void call_get_optional_const_ref_bad(Arr x) {
  x.arr[0] = 42;
  get_optional_const_ref(x);
}

void call_get_optional_const_ref_ok(Arr x) {
  x.arr[0] = 42;
  get_optional_const_ref(std::move(x));
}

void get_optional_value(std::optional<Arr> x) {}

void call_get_optional_value_bad(Arr x) {
  x.arr[0] = 42;
  get_optional_value(x);
}

void call_get_optional_value_ok(Arr x) {
  x.arr[0] = 42;
  get_optional_value(std::move(x));
}

void call_get_optional_value_static_cast_ok(Arr x) {
  x.arr[0] = 42;
  get_optional_value(static_cast<Arr&&>(x));
}

class OptionalFieldOk {
 public:
  template <typename T>
  OptionalFieldOk(T&& x) : x(std::forward<T>(x)) {}

 private:
  std::optional<Arr> x;
};

void construct_optional_field(Arr x) { OptionalFieldOk o(std::move(x)); }

void call_std_distance_ok(const std::string& s) {
  std::string t = s;
  t.data()[0] = 'a'; // t is modified
  std::distance(t.begin(), t.end());
}

void cheap_directory_iterator_ok(const std::string& path) {
  for (auto const& entry : std::filesystem::directory_iterator{path}) {
  }
}

void structured_binding_bad(Arr x) {

  int y = 0;
  auto tuple = std::tie(x, y); // Create a tuple-like object using std::tie
  auto [first, second] = tuple;
  auto c = first; // copy
}

void structured_binding_loop_bad_FN(std::map<int, std::string> mapEntity) {
  for (auto [keyActual, mapValue] : mapEntity) {
    // need to model std::get to handle this example
  }
}

struct Pair {
  Arr first;
  Arr second;
};

void structured_binding_no_holding_var_bad(Pair myPair) {
  auto [l1, l2] = myPair;
  auto c = l1; // can detect the copy
}

void use_recursive_directory_iterator_ok(const std::string& root) {
  for (auto& p : std::filesystem::recursive_directory_iterator(root)) {
  }
}

class OptionalTiriviallyCopyable_ok {
  std::optional<std::string> f;

  OptionalTiriviallyCopyable_ok(std::string_view x) : f(x) {}
};
} // namespace unnecessary_copy
