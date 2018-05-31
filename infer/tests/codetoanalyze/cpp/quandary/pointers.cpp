/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <string>
#include <iostream>

extern std::string* __infer_taint_source();
extern void __infer_taint_sink(std::string);

namespace pointers {

void assign_pointer_to_source(std::string* pointer) {
  *pointer = *__infer_taint_source();
}

void assign_pointer_pass_to_sink_bad1(std::string* pointer) {
  assign_pointer_to_source(pointer);
  __infer_taint_sink(*pointer);
}

void assign_pointer_pass_to_sink_bad2() {
  std::string* pointer = new std::string();
  assign_pointer_to_source(pointer);
  __infer_taint_sink(*pointer);
}

void assign_source_by_reference(std::string& reference) {
  reference = *__infer_taint_source();
}

void assign_source_by_reference_bad1() {
  std::string local;
  assign_source_by_reference(local);
  __infer_taint_sink(local);
}

void assign_source_by_reference_bad2(std::string formal) {
  assign_source_by_reference(formal);
  __infer_taint_sink(formal);
}

void call_assign_source_by_reference(std::string& formal) {
  assign_source_by_reference(formal);
}

void assign_source_by_reference_bad3() {
  std::string local;
  call_assign_source_by_reference(local);
  __infer_taint_sink(local);
}

void reuse_pointer_as_local(std::string* pointer) {
  pointer = __infer_taint_source();
  std::cout << *pointer;
}

// need to understand that assigning a reference doesn't change the value in the
// caller
void FP_reuse_pointer_as_local_ok(std::string* pointer) {
  reuse_pointer_as_local(pointer);
  __infer_taint_sink(*pointer);
}

void pointer_arithmetic_ok1(int* i) { *(i + 1) = 7; }

void pointer_arithmetic_ok2(int* i) { *(2 + 7 + 5 + i + 1) = 7; }
} // namespace pointers
