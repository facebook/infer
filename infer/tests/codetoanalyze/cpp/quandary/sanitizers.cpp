/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <string>

extern std::string __infer_taint_source();

extern void __infer_sql_sink(std::string);
extern void __infer_taint_sink(std::string);
extern void __infer_url_sink(std::string);

extern std::string __infer_all_sanitizer(std::string);
extern std::string __infer_shell_sanitizer(std::string);
extern std::string __infer_sql_sanitizer(std::string);
extern std::string __infer_url_sanitizer(std::string);

namespace sanitizers {

void escape_sql_to_sql_ok() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_sql_sanitizer(source);
  __infer_sql_sink(sanitized);
}

void escape_shell_to_shell_ok() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_shell_sanitizer(source);
  system(sanitized.c_str());
}

void escape_url_to_url_ok() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_url_sanitizer(source);
  __infer_url_sink(sanitized);
}

void foo(std::string sanitized) { __infer_url_sink(sanitized); }

void all_to_all_ok() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_all_sanitizer(source);
  __infer_taint_sink(sanitized);
}

// test a few permutations of "wrong sanitizer for this sink"

void escape_sql_to_shell_bad() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_sql_sanitizer(source);
  system(sanitized.c_str());
}

void escape_sql_to_url_bad() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_sql_sanitizer(source);
  __infer_url_sink(sanitized);
}

void escape_shell_to_url_bad() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_shell_sanitizer(source);
  __infer_url_sink(sanitized);
}

void escape_url_to_sql_bad() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_url_sanitizer(source);
  __infer_sql_sink(sanitized);
}

void dead_sanitizer_bad() {
  auto source = __infer_taint_source();
  auto sanitized = __infer_all_sanitizer(source);
  __infer_taint_sink(source); // the sink does not use the sanitized value;
                              // report
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
  auto y = __infer_sql_sanitizer(x);
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
    x = __infer_sql_sanitizer(source);
  }
  __infer_sql_sink(x);
}

} // namespace sanitizers
