/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <string>

extern void* __infer_taint_source();
extern void __infer_taint_sink(void*);

namespace basics {

class Obj {
 public:
  void* method_source() { return (void*)0; }
  void method_sink(void*) {}
  static void* static_source() { return (void*)0; }
  static void static_sink(void*) {}
  std::string string_source(int i) { return ""; }
  static int taint_arg_source(int* arg) { return 1; }
  void string_sink(std::string) {}
  static std::string* sanitizer1(std::string* input) { return input; }
  static std::string sanitizer2(const std::string& input) { return input; }

  std::string field1;
  std::string field2;

  void endpoint(std::string source1, void* source2) {
    this->string_sink(source1);
    __infer_taint_sink(source2);
  }
};

void* returnSource() { return __infer_taint_source(); }

void callSink(void* param) { __infer_taint_sink(param); }

void* id(void* param) { return param; }

void sourceToSinkDirectBad() {
  void* source = __infer_taint_source();
  __infer_taint_sink(source);
}

void returnSourceToSinkBad() {
  void* source = returnSource();
  __infer_taint_sink(source);
}

void sourceThenCallSinkBad() {
  void* source = __infer_taint_source();
  callSink(source);
}

void propagateBad() {
  void* source = __infer_taint_source();
  void* launderedSource = id(source);
  callSink(launderedSource);
}

// make sure specifying external sources/sinks as instance methods works
void object_source_sink_bad(Obj obj) {
  void* source = obj.method_source();
  obj.method_sink(source);
}

// make sure specifying external sources/sinks as static methods works
void static_source_sink_bad(Obj obj) {
  void* source = Obj::static_source();
  Obj::static_sink(source);
}

template <class T>
T* template_source() {
  return nullptr;
}

template <class T>
void template_sink(T) {}

void template_source_bad() {
  void* source = template_source<void*>();
  __infer_taint_sink(source);
}

void string_source_bad(Obj obj) {
  std::string source = obj.string_source(5);
  obj.string_sink(source);
}

void via_field_bad1() {
  Obj* obj = new Obj();
  obj->field1 = *template_source<std::string>();
  template_sink<std::string>(obj->field1);
}

void via_field_bad2(Obj* obj) {
  obj->field1 = *template_source<std::string>();
  template_sink<std::string>(obj->field1);
}

void via_field_ok1() {
  Obj* obj = new Obj();
  obj->field1 = *template_source<std::string>();
  obj->field1 = nullptr;
  template_sink<std::string>(obj->field1);
}

void via_field_ok2() {
  Obj* obj = new Obj();
  obj->field1 = *template_source<std::string>();
  template_sink<std::string>(obj->field2);
}

template <class T>
T* id1(T* t) {
  return t;
}

template <class T>
T id2(T t) {
  return t;
}

void via_passthrough_bad1(Obj* obj) {
  std::string source = obj->string_source(0);
  std::string* source_ptr = &source;
  std::string* laundered_source = id1<std::string>(source_ptr);
  obj->string_sink(*laundered_source);
}

void via_passthrough_bad2(Obj* obj) {
  std::string source = obj->string_source(0);
  std::string laundered_source = id2<std::string>(source);
  obj->string_sink(laundered_source);
}

void taint_arg_source_bad() {
  int source;
  Obj::taint_arg_source(&source);
  __infer_taint_sink((void*)source);
}

void taint_arg_source_ok() {
  int source;
  int ret = Obj::taint_arg_source(&source);
  __infer_taint_sink((void*)ret); // return value is not a source
}

void via_sanitizer_ok1(Obj* obj) {
  std::string* source = &obj->string_source(0);
  std::string* sanitized = Obj::sanitizer1(source);
  obj->string_sink(*sanitized);
}

void via_sanitizer_ok2(Obj* obj) {
  std::string source = obj->string_source(0);
  std::string sanitized = obj->sanitizer2(source);
  obj->string_sink(sanitized);
}

std::string* unsanitized_bad(Obj* obj) {
  std::string* source = &obj->string_source(0);
  std::string* sanitized = Obj::sanitizer1(source);
  obj->string_sink(*source);
  return sanitized;
}
}
