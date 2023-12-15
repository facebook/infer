/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdio.h>
#include <string>

extern std::string __infer_taint_source();
extern void __infer_taint_sink(std::string);

// tests related to string manipulation, format strings, etc.
namespace strings {

void sprintf1_bad() {
  char laundered_source[50];
  auto source = __infer_taint_source();
  sprintf(laundered_source, "%s", source.c_str());
  __infer_taint_sink(laundered_source);
}

void sprintf2_bad() {
  char laundered_source[50];
  auto source = __infer_taint_source();
  sprintf(laundered_source, "%s%s%d", "a", source.c_str(), 1);
  __infer_taint_sink(laundered_source);
}

void strcpy1_bad() {
  char laundered_source[50];
  auto source = __infer_taint_source();
  auto copy = strcpy(laundered_source, source.c_str());
  __infer_taint_sink(copy);
}

void strcpy2_bad() {
  char laundered_source[50];
  auto source = __infer_taint_source();
  strcpy(laundered_source, source.c_str());
  __infer_taint_sink(laundered_source);
}

void strncpy_bad() {
  char laundered_source[50];
  auto source = __infer_taint_source();
  strncpy(laundered_source, source.c_str(), 50);
  __infer_taint_sink(laundered_source);
}

void memcpy_bad() {
  char laundered_source[50];
  auto source = __infer_taint_source();
  memcpy(laundered_source, source.c_str(), 50);
  __infer_taint_sink(laundered_source);
}

void memmove_bad() {
  char laundered_source[50];
  auto source = __infer_taint_source();
  auto copy = (char*)memmove(laundered_source, source.c_str(), 50);
  __infer_taint_sink(copy);
}

void memchr_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = (char*)memchr(source.c_str(), 'a', 10);
  __infer_taint_sink(laundered_source);
}

void constructor1_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = std::string(source);
  __infer_taint_sink(laundered_source);
}

void constructor2_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = std::string(source, 0, 5);
  __infer_taint_sink(laundered_source);
}

void constructor3_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = std::string(source.begin(), source.begin() + 5);
  __infer_taint_sink(laundered_source);
}

void concat1_bad() {
  auto source = __infer_taint_source();
  source += "other string";
  __infer_taint_sink(source);
}

void concat2_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = std::string("string");
  laundered_source += source;
  __infer_taint_sink(laundered_source);
}

void concat3_bad() {
  auto source = __infer_taint_source();
  __infer_taint_sink(source += "string");
}

void append1_bad() {
  auto source = __infer_taint_source();
  __infer_taint_sink(std::string("string").append(source));
}

void append2_bad() {
  auto source = __infer_taint_source();
  source.append("string");
  __infer_taint_sink(source);
}

void assign1_bad() {
  auto source = __infer_taint_source();
  __infer_taint_sink(std::string("string").assign(source));
}

void assign2_bad() {
  auto source = __infer_taint_source();
  source.assign("string");
  __infer_taint_sink(source);
}

void insert1_bad() {
  auto source = __infer_taint_source();
  __infer_taint_sink(std::string("string").assign(source));
}

void insert2_bad() {
  auto source = __infer_taint_source();
  source.insert(0, "string");
  __infer_taint_sink(source);
}

void replace1_bad() {
  auto source = __infer_taint_source();
  __infer_taint_sink(std::string("string").replace(0, 5, source));
}

void replace2_bad() {
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

void FN_format1_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = format1("%s", source).str();
  __infer_taint_sink(laundered_source);
}

void FN_format2_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = format2("%s", source)->str();
  __infer_taint_sink(laundered_source);
}

void format3_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = format3("%s", source).str();
  __infer_taint_sink(laundered_source);
}

void format4_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = format4("%s", source)->str();
  __infer_taint_sink(laundered_source);
}

void format_varargs_bad() {
  auto source = __infer_taint_source();
  auto laundered_source = format3("%s%s", "a", source, "b").str();
  __infer_taint_sink(laundered_source);
}
} // namespace strings
