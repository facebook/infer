/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <algorithm>
#include <cstring>
#include <iostream>
#include <string>
#include <utility>

// inspired by folly::Range
struct Range {
  const char *b_, *e_;

  Range(const std::string& str) : b_(str.data()), e_(b_ + str.size()) {}

  char operator[](size_t i) { return b_[i]; }
};

const Range setLanguage(const std::string& s) {
  return s[0] == 'k' ? s.substr(0, 1) // cast to Range returns pointers
                                      // into stack-allocated temporary string
                     : "en";
}

bool use_range_of_invalidated_temporary_string_bad(const std::string& str) {
  auto s = setLanguage(str);
  return s[0] == 'k';
}

void some_function(std::string s);

int string_passed_as_param_ok() {
  std::string str = "";
  some_function(str);
  if (str.empty()) {
    return 0;
  }
  return 1;
}

void copy_string_bad() {
  std::string x = "abc";
  std::string y = x;
  if (y.length() == 3) {
    int* p = nullptr;
    *p = 42;
  }
}

void copy_string_ok() {
  std::string x = "abc";
  std::string y = x;
  if (y.length() != 3) {
    int* p = nullptr;
    *p = 42;
  }
}

std::string make_string();

size_t c_str_of_temporary_bad() {
  const char* p = make_string().c_str();
  return strlen(p);
}

size_t c_str_of_temporary_in_full_expression_ok() {
  return strlen(make_string().c_str());
}

size_t c_str_of_lifetime_extended_temporary_ok() {
  const std::string& s = make_string();
  const char* p = s.c_str();
  return strlen(p);
}

char c_str_after_scope_bad() {
  const char* p;
  {
    std::string s("hello");
    p = s.c_str();
  }
  return *p;
}

const char* c_str_of_local() {
  std::string s("hello");
  return s.c_str();
}

char use_c_str_of_local_bad() { return *c_str_of_local(); }

char data_then_append_bad(std::string& s) {
  const char* p = s.data();
  s.append(1000, 'x');
  return *p;
}

char c_str_then_push_back_bad(std::string& s) {
  const char* p = s.c_str();
  s.push_back('x');
  return *p;
}

char c_str_then_plus_assign_bad(std::string& s) {
  const char* p = s.c_str();
  s += "more";
  return *p;
}

char c_str_then_assign_bad(std::string& s, const std::string& t) {
  const char* p = s.c_str();
  s = t;
  return *p;
}

char c_str_then_clear_bad(std::string& s) {
  const char* p = s.c_str();
  s.clear();
  return *p;
}

char c_str_then_insert_bad(std::string& s) {
  const char* p = s.c_str();
  s.insert(0, "more");
  return *p;
}

char c_str_then_reserve_bad(std::string& s) {
  const char* p = s.c_str();
  s.reserve(1000);
  return *p;
}

void append_to(std::string& s) { s.append(1000, 'x'); }

char c_str_then_append_in_callee_bad(std::string& s) {
  const char* p = s.c_str();
  append_to(s);
  return *p;
}

char c_str_then_erase_bad(std::string& s) {
  const char* p = s.c_str();
  s.erase(0, 1);
  return *p;
}

char c_str_then_pop_back_bad(std::string& s) {
  const char* p = s.c_str();
  s.pop_back();
  return *p;
}

char c_str_after_reserve_then_append_bad(std::string& s) {
  s.reserve(100);
  const char* p = s.c_str();
  s += "abc";
  return *p;
}

char c_str_of_append_result_then_push_back_bad(std::string& s) {
  const char* p = s.append("b").c_str();
  s.push_back('c');
  return *p;
}

char c_str_of_append_result_ok(std::string& s) {
  const char* p = s.append("b").c_str();
  return *p;
}

char c_str_after_append_ok(std::string& s) {
  s.append(1000, 'x');
  const char* p = s.c_str();
  return *p;
}

char c_str_then_const_calls_ok(const std::string& s) {
  const char* p = s.c_str();
  if (s.empty() || s.length() == 0 || s.size() == 0) {
    return 0;
  }
  return *p;
}

char c_str_then_append_other_string_ok(std::string& s, std::string& t) {
  const char* p = s.c_str();
  t.append("x");
  return *p;
}

std::string copy_then_append_ok(const std::string& s) {
  std::string t = s;
  t.append("x");
  return t;
}

char begin_then_append_bad(std::string& s) {
  auto it = s.begin();
  s.append(1000, 'x');
  return *it;
}

char begin_again_after_append_ok(std::string& s) {
  auto it = s.begin();
  s.append(1000, 'x');
  it = s.begin();
  return *it;
}

char find_then_append_bad(std::string& s) {
  auto it = std::find(s.begin(), s.end(), 'a');
  s.append(1000, 'x');
  return *it;
}

void erase_in_loop_ok(std::string& s) {
  for (auto it = s.begin(); it != s.end();) {
    if (*it == '\r') {
      it = s.erase(it);
    } else {
      ++it;
    }
  }
}

void insert_in_loop_ok(std::string& s) {
  for (auto it = s.begin(); it != s.end(); ++it) {
    if (*it == '\n') {
      it = s.insert(it, '\r');
      ++it;
    }
  }
}

char assigned_begin_after_scope_bad() {
  std::string::iterator it;
  {
    std::string s("hello");
    it = s.begin();
  }
  return *it;
}

int sum_chars(const std::string& s) {
  int n = 0;
  for (char c : s) {
    n += c;
  }
  return n;
}

int iterate_copy_of_c_str_after_clear_ok(std::string& s) {
  std::string t(s.c_str());
  s.clear();
  return sum_chars(t);
}

int iterate_copy_of_data_after_scope_ok() {
  std::string* t;
  {
    std::string s("hello");
    t = new std::string(s.data(), s.size());
  }
  int n = sum_chars(*t);
  delete t;
  return n;
}

// operator[] is not tied to the modelled buffer
char FN_subscript_then_append_bad(std::string& s) {
  const char* p = &s[0];
  s.append(1000, 'x');
  return *p;
}

// swap() is not modelled
char FN_c_str_then_swap_bad(std::string& s, std::string& t) {
  const char* p = s.c_str();
  s.swap(t);
  return *p;
}

// swap() is not modelled
char FN_c_str_then_std_swap_bad(std::string& s, std::string& t) {
  const char* p = s.c_str();
  std::swap(s, t);
  return *p;
}

// only the buffer of the assigned string is invalidated, not the one of the
// source
char FN_c_str_of_moved_from_string_bad(std::string& s, std::string& t) {
  const char* p = t.c_str();
  s = std::move(t);
  return *p;
}

// only member functions of the string invalidate its buffer
char FN_c_str_then_getline_bad(std::string& s) {
  const char* p = s.c_str();
  std::getline(std::cin, s);
  return *p;
}

// only member functions of the string invalidate its buffer
char FN_c_str_then_extract_bad(std::string& s) {
  const char* p = s.c_str();
  std::cin >> s;
  return *p;
}

// the iterator returned by cbegin() is not tied to the buffer
char FN_cbegin_then_append_bad(std::string& s) {
  auto it = s.cbegin();
  s.append(1000, 'x');
  return *it;
}

// the iterator returned by end() is not tied to the buffer
char FN_end_then_reserve_bad(std::string& s) {
  if (s.empty()) {
    return 0;
  }
  auto it = s.end();
  s.reserve(100);
  return *(it - 1);
}

// pointer arithmetic loses the link to the buffer
char FN_data_plus_one_then_append_bad(std::string& s) {
  if (s.size() < 2) {
    return 0;
  }
  const char* p = s.data() + 1;
  s.append(1000, 'x');
  return *p;
}
