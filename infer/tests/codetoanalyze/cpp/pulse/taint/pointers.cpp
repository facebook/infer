/*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

void reuse_pointer_as_local_ok(std::string* pointer) {
  reuse_pointer_as_local(pointer);
  __infer_taint_sink(*pointer);
}

void __infer_taint_sink1(std::string) {}

void funptr_bad0_FN() {
  auto f = __infer_taint_sink;
  f(*(__infer_taint_source()));
}

void funptr_helper_bad1_FN(void (*sink)(std::string)) {
  sink(*(__infer_taint_source()));
}

void funptr1() {
  auto f = __infer_taint_sink;
  funptr_helper_bad1_FN(f);
}

void funptr_helper_bad2_FN(std::string* (*source)()) {
  __infer_taint_sink(*(source()));
}

void funptr2() { funptr_helper_bad2_FN(__infer_taint_source); }

void pointer_arithmetic_ok1(int* i) { *(i + 1) = 7; }

void pointer_arithmetic_ok2(int* i) { *(2 + 7 + 5 + i + 1) = 7; }
} // namespace pointers
