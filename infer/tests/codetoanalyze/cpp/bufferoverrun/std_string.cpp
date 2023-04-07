/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <cstdarg>
#include <string>

char last_char1_Bad(char* s, int i) {
  char buf[1024];
  int n = snprintf(buf, sizeof(buf), "%s%d", s, i);
  return buf[n - 1];
}

char last_char1_Good(char* s, int i) {
  char buf[1024];
  int n = snprintf(buf, sizeof(buf), "%s%d", s, i);
  if (n > 0 && n <= sizeof(buf)) {
    return buf[n - 1];
  } else {
    return '\0';
  }
}

char last_char2_Bad(const char* fmt, ...) {
  char buf[1024];
  va_list args;
  va_start(args, fmt);
  int n = vsnprintf(buf, sizeof(buf), fmt, args);
  va_end(args);
  return buf[n - 1];
}

std::string to_string1_Bad(char* s, int i) {
  char buf[1024];
  int n = snprintf(buf, sizeof(buf), "%s%d", s, i);
  return std::string(buf, n);
}

std::string to_string1_Good(char* s, int i) {
  char buf[1024];
  int n = snprintf(buf, sizeof(buf), "%s%d", s, i);
  if (n < 0) {
    return NULL;
  } else if (n > sizeof(buf)) {
    n = sizeof(buf);
  }
  return std::string(buf, n);
}

std::string to_string2_Bad(const char* fmt, ...) {
  char buf[1024];
  va_list args;
  va_start(args, fmt);
  int n = vsnprintf(buf, sizeof(buf), fmt, args);
  va_end(args);
  return std::string(buf, n);
}

std::string to_string2_Good(const char* fmt, ...) {
  char buf[1024];
  va_list args;
  va_start(args, fmt);
  int n = vsnprintf(buf, sizeof(buf), fmt, args);
  va_end(args);
  if (n < 0) {
    return NULL;
  } else if (n > sizeof(buf)) {
    n = sizeof(buf);
  }
  return std::string(buf, n);
}

/* Inferbo's model ignores the encoding errors for less noise of
   analysis results.  While [vsnprintf] returns a nagative number when
   an encoding error, it is less likely to lead to a security
   problem.  */
std::string to_string3_Bad_FN(const char* fmt, ...) {
  char buf[1024];
  va_list args;
  va_start(args, fmt);
  int n = vsnprintf(buf, sizeof(buf), fmt, args);
  va_end(args);
  if (n > sizeof(buf)) {
    n = sizeof(buf);
  }
  return std::string(buf, n);
}

void empty_Good(std::string s) {
  if (s.empty()) {
    if (!s.empty()) {
      int a[10];
      a[10] = 0;
    }
  }
}

void empty_Bad(std::string s) {
  if (s.empty()) {
    int a[10];
    a[10] = 0;
  }
}

void length_Good() {
  std::string s("hello");
  int a[10];
  a[s.length()] = 0;
}

void length_Bad() {
  std::string s("hellohello");
  int a[10];
  a[s.length()] = 0;
}

void length2_Good() {
  const char* c = "hello";
  std::string s(c);
  int a[10];
  a[s.length()] = 0;
}

void length2_Bad() {
  const char* c = "hellohello";
  std::string s(c);
  int a[10];
  a[s.length()] = 0;
}

void length3_Good() {
  char* c = "hello";
  std::string s(c);
  int a[10];
  a[s.length()] = 0;
}

void length3_Bad() {
  char* c = "hellohello";
  std::string s(c);
  int a[10];
  a[s.length()] = 0;
}

void length4(char* c) {
  std::string s(c);
  int a[10];
  a[s.length()] = 0;
}

void call_length4_1_Good() {
  char* c = "hello";
  length4(c);
}

void call_length4_1_Bad() {
  char* c = "hellohello";
  length4(c);
}

void call_length4_2_Good() { length4("hello"); }

void call_length4_2_Bad() { length4("hellohello"); }

void size_Good() {
  std::string s("hello");
  int a[10];
  a[s.size()] = 0;
}

void size_Bad() {
  std::string s("hellohello");
  int a[10];
  a[s.size()] = 0;
}

void compare_Good_FP(std::string s) {
  if (s.compare(0, s.size(), s) != 0) {
    int a[10];
    a[10] = 0;
  }
}

void compare_Bad(std::string s) {
  if (s.compare(0, s.size(), s) == 0) {
    int a[10];
    a[10] = 0;
  }
}

void equal_Good_FP(std::string s) {
  if (s != s) {
    int a[10];
    a[10] = 0;
  }
}

void equal_Bad() {
  std::string s1("hello");
  std::string s2("hello");
  if (s1 == s2) {
    int a[10];
    a[10] = 0;
  }
}

constexpr char const_s[] = "const_s";

void equal2_Good_FP() {
  std::string s(const_s);
  if (s != const_s) {
    int a[10];
    a[10] = 0;
  }
}
