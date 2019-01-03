/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
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
