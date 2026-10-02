/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <iterator>
#include <memory>
#include <vector>

void fill_n_Bad() {
  int a[4];
  std::fill_n(a, 8, 0);
}

void fill_n_Good() {
  int a[4];
  std::fill_n(a, 4, 0);
}

void fill_n_offset_Bad() {
  int a[4];
  std::fill_n(a + 2, 3, 0);
}

void fill_n_address_Good() {
  int a[4];
  std::fill_n(&a[1], 3, 0);
}

void fill_n_address_Bad() {
  int a[4];
  std::fill_n(&a[1], 4, 0);
}

void fill_n_row_Good() {
  int m[4][4];
  std::fill_n(m[3], 4, 0);
}

void fill_n_row_Bad() {
  int m[4][4];
  std::fill_n(m[3], 5, 0);
}

void fill_n_bytes_Good() {
  int a[4];
  std::fill_n(reinterpret_cast<char*>(a), sizeof(a), 0);
}

void fill_n_bytes_Bad() {
  int a[4];
  std::fill_n(reinterpret_cast<char*>(a), sizeof(a) + 1, 0);
}

void fill_n_double_as_float_Good() {
  double d[4];
  std::fill_n(reinterpret_cast<float*>(d), 8, 0.f);
}

void fill_n_double_as_float_Bad() {
  double d[4];
  std::fill_n(reinterpret_cast<float*>(d), 9, 0.f);
}

struct Rec {
  double a;
  double b;
  double c;
};

void fill_n_struct_as_double_Good() {
  Rec r[2];
  std::fill_n(
      reinterpret_cast<double*>(r), 2 * sizeof(Rec) / sizeof(double), 0.0);
}

struct Vec3 {
  float x;
  float y;
  float z;
};

void copy_n_vec3_float_view_Good() {
  Vec3 v[4];
  float out[12];
  std::copy_n(reinterpret_cast<const float*>(v), 12, out);
}

void copy_n_floats_into_chars_Bad() {
  float src[5] = {};
  char buf[16];
  std::copy_n(src, 5, reinterpret_cast<float*>(buf));
}

void fill_floats(float* p, int n) { std::fill_n(p, n, 0.f); }

// Casts to non-integer pointers do not recount arrays, so fill_floats gets the
// length of d in doubles.
void FP_call_fill_floats_double_Good() {
  double d[4];
  fill_floats(reinterpret_cast<float*>(d), 8);
}

struct Header {
  int magic;
  int length;
};

void clear_header(Header* h) {
  std::fill_n(reinterpret_cast<char*>(h), sizeof(Header), 0);
}

// As with memset, the byte size of an object allocated by new is unknown, which
// gives BUFFER_OVERRUN_L5.
void FP_call_clear_header_Good() {
  Header* h = new Header;
  clear_header(h);
  delete h;
}

void fill_n_new_header_bytes_Good() {
  Header* h = new Header;
  std::fill_n(reinterpret_cast<char*>(h), sizeof(Header), 0);
  delete h;
}

void fill_n_array_reference(int (&a)[4]) { std::fill_n(a, 8, 0); }

void call_fill_n_array_reference_Bad() {
  int a[4];
  fill_n_array_reference(a);
}

void fill_n_negative_count_Good() {
  int a[4];
  std::fill_n(a, -1, 0);
}

void fill_n_malloc_Bad() {
  int* p = (int*)malloc(4 * sizeof(int));
  if (p != nullptr) {
    std::fill_n(p, 8, 0);
    free(p);
  }
}

void fill_n_contents_Good() {
  int idx[2];
  std::fill_n(idx, 2, 9);
  int a[10];
  a[idx[0]] = 0;
}

void fill_n_contents_Bad() {
  int idx[2];
  std::fill_n(idx, 2, 10);
  int a[10];
  a[idx[0]] = 0;
}

void fill_n_return_Good() {
  int a[4];
  int* end = std::fill_n(a, 3, 0);
  *end = 0;
}

void fill_n_return_Bad() {
  int a[4];
  int* end = std::fill_n(a, 4, 0);
  *end = 0;
}

void fill_n_param(int* p, int n) { std::fill_n(p, n, 0); }

void call_fill_n_param_Good() {
  int a[4];
  fill_n_param(a, 4);
}

void call_fill_n_param_Bad() {
  int a[4];
  fill_n_param(a, 8);
}

void fill_Bad() {
  int a[4];
  std::fill(a, a + 8, 0);
}

void fill_Good() {
  int a[4];
  std::fill(a, a + 4, 0);
}

void strncpy_after_fill_n_Bad() {
  char src[40] = "abc";
  std::fill_n(src, sizeof(src), 'a');
  char dst[100];
  strncpy(dst, src, 44);
}

void strncpy_after_empty_fill_n_Good() {
  char src[40] = "abc";
  std::fill_n(src, 0, 'a');
  char dst[100];
  strncpy(dst, src, 44);
}

// Overrunning the row m[0] is undefined behavior, although the rows are
// contiguous.
void fill_flat_2d_array_Bad() {
  int m[4][4];
  std::fill(&m[0][0], &m[0][0] + 16, 0);
}

void copy_Bad() {
  int src[8] = {};
  int a[4];
  std::copy(src, src + 8, a);
}

void copy_Good() {
  int src[8] = {};
  int a[4];
  std::copy(src, src + 4, a);
}

void copy_src_Bad() {
  int src[4] = {};
  int a[8];
  std::copy(src, src + 8, a);
}

void copy_param_Bad(const int* src) {
  int a[4];
  std::copy(src, src + 8, a);
}

void copy_address_Bad(const int* src) {
  int a[4];
  std::copy(src, src + 4, &a[1]);
}

void copy_pointers_Good() {
  const char* names[4] = {"a", "b", "c", "d"};
  const char* out[3];
  std::copy(&names[1], &names[4], out);
}

void copy_header_bytes(const Header* h) {
  char buf[sizeof(Header)];
  const char* p = reinterpret_cast<const char*>(h);
  std::copy(p, p + sizeof(Header), buf);
}

void call_copy_header_bytes_Good() {
  Header h[2] = {};
  copy_header_bytes(&h[1]);
}

void copy_contents_Good() {
  int src[2] = {9, 9};
  int idx[2];
  std::copy(src, src + 2, idx);
  int a[10];
  a[idx[0]] = 0;
}

void copy_contents_Bad() {
  int src[2] = {10, 10};
  int idx[2];
  std::copy(src, src + 2, idx);
  int a[10];
  a[idx[0]] = 0;
}

void copy_empty_range_contents_Good() {
  int src[2] = {10, 10};
  int idx[2] = {0, 0};
  std::copy(src, src, idx);
  int a[10];
  a[idx[0]] = 0;
}

void strncpy_after_copy_Bad(const char* in) {
  char src[40] = "abc";
  std::copy(in, in + sizeof(src), src);
  char dst[100];
  strncpy(dst, src, 44);
}

void copy_return_Good() {
  char src[8] = {};
  char buf[8];
  char* end = std::copy(src, src + 7, buf);
  *end = '\0';
}

void copy_return_Bad() {
  char src[8] = {};
  char buf[8];
  char* end = std::copy(src, src + 8, buf);
  *end = '\0';
}

struct Pair {
  int x;
  int y;
};

void copy_struct_Bad() {
  Pair src[8];
  Pair a[4];
  std::copy(src, src + 8, a);
}

void copy_n_Bad(const int* src) {
  int a[4];
  std::copy_n(src, 8, a);
}

void copy_n_Good(const int* src) {
  int a[4];
  std::copy_n(src, 4, a);
}

void copy_n_src_Bad() {
  int src[4] = {};
  int a[8];
  std::copy_n(src, 8, a);
}

void move_Bad() {
  int src[8] = {};
  int a[4];
  std::move(src, src + 8, a);
}

void move_Good() {
  int src[8] = {};
  int a[4];
  std::move(src, src + 4, a);
}

void fill_n_back_inserter_Good(std::vector<int>& v) {
  std::fill_n(std::back_inserter(v), 8, 0);
}

void copy_back_inserter_Good(const int* src, std::vector<int>& v) {
  std::copy(src, src + 8, std::back_inserter(v));
}

// Only raw pointer iterators are modeled.
void FN_copy_vector_iterator_Bad() {
  std::vector<int> v(8);
  int a[4];
  std::copy(v.begin(), v.end(), a);
}

// std::begin and std::end are not modeled, so the overrun is only reported as
// BUFFER_OVERRUN_U5.
void FN_copy_begin_end_Bad() {
  int src[5] = {};
  int a[4];
  std::copy(std::begin(src), std::end(src), a);
}

// The following algorithms are not modeled.
void FN_copy_backward_Bad() {
  int src[8] = {};
  int a[4];
  std::copy_backward(src, src + 8, a + 4);
}

void FN_move_backward_Bad() {
  int src[8] = {};
  int a[4];
  std::move_backward(src, src + 8, a + 4);
}

void FN_uninitialized_copy_Bad() {
  int src[8] = {};
  int a[4];
  std::uninitialized_copy(src, src + 8, a);
}

void FN_uninitialized_fill_Bad() {
  int a[4];
  std::uninitialized_fill(a, a + 8, 0);
}
