/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <map>
#include <memory>
#include <string>

extern std::string __infer_taint_source();
extern void __infer_taint_sink(std::string);
extern std::string skip_value(std::string);
extern std::string* skip_pointer(std::string);
extern void skip_by_ref(std::string, std::string&);

extern int of_string(std::string);

namespace unknown_code {

void direct_bad() {
  auto source = __infer_taint_source();
  __infer_taint_sink(source);
}

void skip_value_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = skip_value(source);
  __infer_taint_sink(laundered_source);
}

void skip_pointer_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = skip_pointer(source);
  __infer_taint_sink(*laundered_source);
}

std::string skip_indirect(std::string formal) {
  auto skipped_pointer = skip_pointer(formal);
  return skip_value(*skipped_pointer);
}

void FN_skip_indirect_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = skip_indirect(source);
  __infer_taint_sink(laundered_source);
}

// for now, we don't have any heuristics for guessing that laundered_by_ref is
// assigned by ref in
// the skipped function
void FN_via_skip_by_ref_bad() {
  auto source = __infer_taint_source();
  std::string laundered_by_ref;
  skip_by_ref(source, laundered_by_ref);
  __infer_taint_sink(laundered_by_ref);
}

} // namespace unknown_code
