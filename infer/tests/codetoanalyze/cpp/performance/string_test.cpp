/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <string>

namespace google {
int StrLen(char*);
}

void call_google_strlen_linear(char* str) { int len = google::StrLen(str); }

void call_google_strlen_with_loop_linear(char* str) {
  int len = google::StrLen(str);
  for (int i = 0; i < len; i++) {
  }
}

int string_compare_linear(const std::string& a, const std::string& b) {
  if (a != b) {
    return 0;
  }
  return 1;
}

void iterate_over_string(const std::string& str) {
  for (char const& c : str) {
  }
}
