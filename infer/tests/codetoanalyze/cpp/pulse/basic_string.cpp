/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <string>

// inspired by folly::Range
struct Range {
  const char *b_, *e_;

  Range(const std::string& str) : b_(str.data()), e_(b_ + str.size()) {}

  char operator[](size_t i) { return b_[i]; }
};

const Range setLanguage(const std::string& s) {
  return s[0] == 'k' ? s.substr(0, 1) // cast to Range returns pointers
                                      // into stack-allocated temporary string
                     : "en";
}

bool use_range_of_invalidated_temporary_string_bad(const std::string& str) {
  auto s = setLanguage(str);
  return s[0] == 'k';
}

void some_function(std::string s);

int string_passed_as_param_ok() {
  std::string str = "";
  some_function(str);
  if (str.empty()) {
    return 0;
  }
  return 1;
}

void copy_string_bad() {
  std::string x = "abc";
  std::string y = x;
  if (y.length() == 3) {
    int* p = nullptr;
    *p = 42;
  }
}

void copy_string_ok() {
  std::string x = "abc";
  std::string y = x;
  if (y.length() != 3) {
    int* p = nullptr;
    *p = 42;
  }
}
