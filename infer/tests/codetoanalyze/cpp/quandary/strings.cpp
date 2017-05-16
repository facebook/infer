/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdio.h>
#include <string>

extern std::string __infer_taint_source();
extern void __infer_taint_sink(std::string);

// tests related to string manipulation, format strings, etc.
namespace strings {

void sprintf_bad1() {
  char laundered_source[50];
  auto source = __infer_taint_source().c_str();
  sprintf(laundered_source, "%s", source);
  __infer_taint_sink(laundered_source);
}

void sprintf_bad2() {
  char laundered_source[50];
  auto source = __infer_taint_source().c_str();
  sprintf(laundered_source, "%s%s%d", "a", source, 1);
  __infer_taint_sink(laundered_source);
}

void strcpy_bad1() {
  char laundered_source[50];
  auto source = __infer_taint_source().c_str();
  auto copy = strcpy(laundered_source, source);
  __infer_taint_sink(copy);
}

void strcpy_bad2() {
  char laundered_source[50];
  auto source = __infer_taint_source().c_str();
  strcpy(laundered_source, source);
  __infer_taint_sink(laundered_source);
}

void strncpy_bad() {
  char laundered_source[50];
  auto source = __infer_taint_source().c_str();
  strncpy(laundered_source, source, 50);
  __infer_taint_sink(laundered_source);
}

void memcpy_bad() {
  char laundered_source[50];
  auto source = __infer_taint_source().c_str();
  memcpy(laundered_source, source, 50);
  __infer_taint_sink(laundered_source);
}

void memmove_bad() {
  char laundered_source[50];
  auto source = __infer_taint_source().c_str();
  auto copy = (char*)memmove(laundered_source, source, 50);
  __infer_taint_sink(copy);
}

void memchr_bad() {
  auto source = __infer_taint_source().c_str();
  auto laundered_source = (char*)memchr(source, 'a', 10);
  __infer_taint_sink(laundered_source);
}

void constructor_bad1() {
  auto source = __infer_taint_source();
  auto laundered_source = std::string(source);
  __infer_taint_sink(laundered_source);
}

void constructor_bad2() {
  auto source = __infer_taint_source();
  auto laundered_source = std::string(source, 0, 5);
  __infer_taint_sink(laundered_source);
}

void constructor_bad3() {
  auto source = __infer_taint_source();
  auto laundered_source = std::string(source.begin(), source.begin() + 5);
  __infer_taint_sink(laundered_source);
}

void concat_bad1() {
  auto source = __infer_taint_source();
  source += "other string";
  __infer_taint_sink(source);
}

void concat_bad2() {
  auto source = __infer_taint_source();
  auto laundered_source = std::string("string");
  laundered_source += source;
  __infer_taint_sink(laundered_source);
}

void concat_bad3() {
  auto source = __infer_taint_source();
  __infer_taint_sink(source += "string");
}

void append_bad1() {
  auto source = __infer_taint_source();
  __infer_taint_sink(std::string("string").append(source));
}

void append_bad2() {
  auto source = __infer_taint_source();
  source.append("string");
  __infer_taint_sink(source);
}

void assign_bad1() {
  auto source = __infer_taint_source();
  __infer_taint_sink(std::string("string").assign(source));
}

void assign_bad2() {
  auto source = __infer_taint_source();
  source.assign("string");
  __infer_taint_sink(source);
}

void insert_bad1() {
  auto source = __infer_taint_source();
  __infer_taint_sink(std::string("string").assign(source));
}

void insert_bad2() {
  auto source = __infer_taint_source();
  source.insert(0, "string");
  __infer_taint_sink(source);
}

void replace_bad1() {
  auto source = __infer_taint_source();
  __infer_taint_sink(std::string("string").replace(0, 5, source));
}

void replace_bad2() {
  auto source = __infer_taint_source();
  source.replace(0, 5, "string");
  __infer_taint_sink(source);
}

void swap_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = std::string("string");
  laundered_source.swap(source);
  __infer_taint_sink(laundered_source);
}

template <class... Args>
class Formatter {

 public:
  explicit Formatter(std::string str, Args&&... args);
  std::string str();
};

template <class... Args>
Formatter<Args...> format1(std::string fmt, Args&&... args) {
  return Formatter<Args...>(fmt, std::forward<Args>(args)...);
}

template <class... Args>
Formatter<Args...>* format2(std::string fmt, Args&&... args) {
  return new Formatter<Args...>(fmt, std::forward<Args>(args)...);
}

template <class... Args>
Formatter<Args...> format3(std::string fmt, Args&&... args);

template <class... Args>
Formatter<Args...>* format4(std::string fmt, Args&&... args);

void format_bad1() {
  auto source = __infer_taint_source();
  auto laundered_source = format1("%s", source).str();
  __infer_taint_sink(laundered_source);
}

void format_bad2() {
  auto source = __infer_taint_source();
  auto laundered_source = format2("%s", source)->str();
  __infer_taint_sink(laundered_source);
}

void format_bad3() {
  auto source = __infer_taint_source();
  auto laundered_source = format3("%s", source).str();
  __infer_taint_sink(laundered_source);
}

void format_bad4() {
  auto source = __infer_taint_source();
  auto laundered_source = format4("%s", source)->str();
  __infer_taint_sink(laundered_source);
}

void format_varargs_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = format3("%s%s", "a", source, "b").str();
  __infer_taint_sink(laundered_source);
}
}
