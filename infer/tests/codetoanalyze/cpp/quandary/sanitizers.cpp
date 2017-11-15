/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdlib.h>
#include <string>

extern std::string __infer_taint_source();

extern void __infer_taint_sink(std::string);
extern void __infer_sql_sink(std::string);

extern std::string __infer_all_sanitizer(std::string);
extern std::string __infer_string_sanitizer(std::string);

namespace sanitizers {

void escape_string_to_sql_ok() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_string_sanitizer(source);
  __infer_sql_sink(sanitized);
}

void escape_string_to_shell_ok() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_string_sanitizer(source);
  system(sanitized.c_str());
}

void escape_string_to_all_bad() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_string_sanitizer(source);
  __infer_taint_sink(sanitized); // wrong kind of sanitizer; report
}

void all_to_all_ok() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_all_sanitizer(source);
  __infer_taint_sink(sanitized);
}

void dead_sanitizer_bad() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_all_sanitizer(source);
  __infer_taint_sink(
      source); // the sink does not use the sanitized value; report
}

void kill_sanitizer_bad() {
  auto source = __infer_taint_source();
  auto x = __infer_all_sanitizer(source);
  x = __infer_taint_source();
  __infer_taint_sink(x);
}

void double_sanitize_ok() {
  auto source = __infer_taint_source();
  auto x = __infer_all_sanitizer(source);
  auto y = __infer_string_sanitizer(x);
  __infer_taint_sink(y);
}

void FN_sanitize_one_branch_bad(bool b) {
  auto source = __infer_taint_source();
  std::string x;
  if (b) {
    x = source;
  } else {
    x = __infer_all_sanitizer(source);
  }
  // we'll fail to report here because sanitizers are a powerset domain
  // ideally they would be an inverted powerset domain, but this is
  // difficult to pull off because our handling of unknown code implicitly
  // relies on the assumption that join should be union
  __infer_taint_sink(x); // should report
}

void sanitize_both_branches_ok(bool b) {
  auto source = __infer_taint_source();
  std::string x;
  if (b) {
    x = __infer_all_sanitizer(source);
  } else {
    x = __infer_all_sanitizer(source);
  }
  __infer_taint_sink(x); // does not report
}

void different_sanitizer_branches_ok(bool b) {
  auto source = __infer_taint_source();
  std::string x;
  if (b) {
    x = __infer_all_sanitizer(source);
  } else {
    x = __infer_string_sanitizer(source);
  }
  __infer_sql_sink(x);
}

} // namespace sanitizers
