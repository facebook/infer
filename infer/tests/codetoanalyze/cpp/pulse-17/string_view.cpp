/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <cstdlib>
#include <cstring>
#include <string>
#include <string_view>

namespace string_view {

std::string make_string();

char view_of_temporary_string_bad() {
  std::string_view sv = make_string();
  return *sv.data();
}

char view_outlives_string_bad() {
  std::string_view sv;
  {
    std::string s = "blah";
    sv = s;
  }
  return *sv.data();
}

char copy_of_view_outlives_string_bad() {
  const char* p;
  {
    std::string s = "blah";
    std::string_view sv = s;
    std::string_view copy(sv);
    p = copy.data();
  }
  return *p;
}

std::string_view view_of(const std::string& s) { return s; }

char view_returned_from_callee_bad() {
  std::string_view sv = view_of(make_string());
  return *sv.data();
}

char deref_view(std::string_view sv) { return *sv.data(); }

char pass_view_of_destroyed_string_bad() {
  std::string_view sv;
  {
    std::string s = "blah";
    sv = s;
  }
  return deref_view(sv);
}

char pass_view_of_temporary_ok() { return deref_view(make_string()); }

char view_of_string_then_append_bad(std::string& s) {
  std::string_view sv = s;
  s.append(1000, 'x');
  return *sv.data();
}

char view_of_freed_buffer_bad() {
  char* buf = (char*)malloc(16);
  if (buf == nullptr) {
    return 0;
  }
  strcpy(buf, "abc");
  std::string_view sv(buf);
  free(buf);
  return *sv.data();
}

char view_with_size_of_deleted_array_bad() {
  char* buf = new char[16];
  buf[0] = 'a';
  std::string_view sv(buf, 1);
  delete[] buf;
  return *sv.data();
}

void view_data_is_string_data_ok(const std::string& s) {
  std::string_view sv = s;
  if (s.data() != sv.data()) {
    int* q = nullptr;
    *q = 42;
  }
}

void view_data_is_char_ptr_ok(const char* p) {
  std::string_view sv(p);
  if (sv.data() != p) {
    int* q = nullptr;
    *q = 42;
  }
}

void view_with_size_data_is_char_ptr_ok(const char* p, size_t n) {
  std::string_view sv(p, n);
  std::string_view copy = sv;
  if (copy.data() != p) {
    int* q = nullptr;
    *q = 42;
  }
}

char view_of_live_string_ok() {
  std::string s = "blah";
  std::string_view sv = s;
  std::string_view copy = sv;
  return *copy.data();
}

char view_of_literal_ok() {
  std::string_view sv = "abc";
  return *sv.data();
}

wchar_t wide_view_of_literal_ok() {
  std::wstring_view sv = L"abc";
  return *sv.data();
}

void string_from_view_keeps_length_ok() {
  std::string_view sv = "abc";
  std::string s(sv);
  if (s.length() != 3) {
    int* q = nullptr;
    *q = 42;
  }
}

void string_from_view_with_size_keeps_length_ok(const char* p) {
  std::string_view sv(p, 3);
  std::string s(sv);
  if (s.length() != 3) {
    int* q = nullptr;
    *q = 42;
  }
}

} // namespace string_view
