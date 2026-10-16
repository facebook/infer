/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <array>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <vector>

int std_array_bo_Bad() {
  std::array<int, 42> a;
  return a[42];
}

int normal_array_bo() {
  int b[42];
  return b[42];
}

void new_char_Good() {
  uint64_t len = 13;
  char* dst;
  dst = new char[len];
}

void new_int1_Bad() {
  uint64_t len = 4611686018427387903; // (1 << 62) - 1
  int32_t* dst;
  dst = new int32_t[len];
}

void new_int2_Bad() {
  uint64_t len = 9223372036854775807; // (1 << 63) - 1
  int32_t* dst;
  dst = new int32_t[len];
}

void new_int3_Bad() {
  uint64_t len = 18446744073709551615; // (1 << 64) - 1
  int32_t* dst;
  dst = new int32_t[len];
}

void std_array_contents_Good() {
  std::array<int, 10> a;
  a[0] = 5;
  a[a[0]] = 0;
}

void std_array_contents_Bad() {
  std::array<int, 10> a;
  a[0] = 10;
  a[a[0]] = 0;
}

void array_iter1_Good() {
  std::array<int, 11> a;
  for (auto it = a.begin(); it < a.end(); ++it) {
    *it = 10;
  }
  a[a[0]] = 0;
}

void array_iter1_Bad() {
  std::array<int, 5> a;
  for (auto it = a.begin(); it < a.end(); ++it) {
    *it = 10;
  }
  a[a[0]] = 0;
}

void array_iter2_Good() {
  std::array<int, 11> a;
  for (auto it = a.begin(); it != a.end(); ++it) {
    *it = 10;
  }
  a[a[0]] = 0;
}

void array_iter2_Bad() {
  std::array<int, 5> a;
  for (auto it = a.begin(); it != a.end(); ++it) {
    *it = 10;
  }
  a[a[0]] = 0;
}

void array_iter3_Good() {
  std::array<int, 11> a = {10};
  for (auto it = a.cbegin(); it < a.cend(); ++it) {
    a[*it] = 10;
  }
}

void array_iter3_Bad() {
  std::array<int, 5> a = {10};
  for (auto it = a.cbegin(); it < a.cend(); ++it) {
    a[*it] = 10;
  }
}

void array_iter_front_Good() {
  std::array<int, 11> a;
  a.front() = 10;
  a[a[0]] = 0;
}

void array_iter_front_Bad() {
  std::array<int, 5> a;
  a.front() = 10;
  a[a[0]] = 0;
}

void array_iter_back_Good() {
  std::array<int, 11> a;
  a.back() = 10;
  a[a[0]] = 0;
}

void array_iter_back_Bad() {
  std::array<int, 5> a;
  a.back() = 10;
  a[a[0]] = 0;
}

void array_rev_iter_Good() {
  std::array<int, 11> a;
  for (auto it = a.rbegin(); it < a.rend(); ++it) {
    *it = 10;
  }
  a[a[0]] = 0;
}

void array_rev_iter_Bad_FN() {
  std::array<int, 5> a;
  for (auto it = a.rbegin(); it < a.rend(); ++it) {
    *it = 10;
  }
  a[a[0]] = 0;
}

void malloc_zero_Bad() { int* a = (int*)malloc(sizeof(int) * 0); }

void new_array_zero_Good() { int* a = new int[0]; }

void array_data_Good() {
  std::array<int, 8> a;
  a.data()[7] = 0;
}

void array_data_Bad() {
  std::array<int, 8> a;
  a.data()[8] = 0;
}

void array_data_memset_Good() {
  std::array<int, 4> a;
  memset(a.data(), 0, sizeof(a));
}

void array_data_memcpy_Bad(const char* src) {
  std::array<int, 4> a;
  memcpy(a.data(), src, 20);
}

void array_elem_addr_fgets_Good(FILE* f) {
  std::array<char, 128> b;
  fgets(&b[0], b.size(), f);
}

void array_elem_addr_fgets_Bad(FILE* f) {
  std::array<char, 128> b;
  fgets(&b[0], 512, f);
}

void array_byte_view_Good() {
  std::array<uint32_t, 4> a;
  uint8_t* p = reinterpret_cast<uint8_t*>(&a[0]);
  p[15] = 0;
}

void array_byte_view_Bad() {
  std::array<uint32_t, 4> a;
  uint8_t* p = reinterpret_cast<uint8_t*>(&a[0]);
  p[16] = 0;
}

void clear_ints(std::array<int, 4>& a, size_t n) { memset(a.data(), 0, n); }

void call_clear_ints_Good() {
  std::array<int, 4> a;
  clear_ints(a, sizeof(a));
}

void call_clear_ints_Bad() {
  std::array<int, 4> a;
  clear_ints(a, sizeof(a) + 1);
}

using Block = std::array<uint32_t, 4>;

void zero_blocks(Block* b, size_t n) { memset(b, 0, n * sizeof(Block)); }

void call_zero_blocks_Good() {
  Block a[3];
  zero_blocks(a, 3);
}

void call_zero_blocks_Bad() {
  Block a[3];
  zero_blocks(a, 4);
}

void copy_key(std::array<uint8_t, 16>& key, const uint8_t* src) {
  memcpy(key.data(), src, 16);
}

void call_copy_key_Good(const uint8_t* src) {
  std::array<uint8_t, 16> keys[4];
  copy_key(keys[1], src);
}

void zero_ref(Block& b, size_t n) { memset(b.data(), 0, n); }

void call_zero_ref_Good() {
  Block a[2];
  zero_ref(a[1], sizeof(Block));
}

void call_zero_ref_Bad() {
  Block a[2];
  zero_ref(a[1], sizeof(Block) + 1);
}

// A reference to a std::array in a C array points into the elements of the
// whole C array, so an overrun into the next std::array is missed.
void FN_call_zero_ref_next_Bad() {
  Block a[3];
  zero_ref(a[1], sizeof(Block) + 1);
}

void zero_next(Block* p, size_t n) { memset((p + 1)->data(), 0, n); }

void call_zero_next_Good() {
  Block a[2];
  zero_next(a, sizeof(Block));
}

void call_zero_next_Bad() {
  Block a[2];
  zero_next(a, sizeof(Block) + 1);
}

void ptr_data_Good() {
  Block a[3];
  Block* p = a;
  p->data()[3] = 0;
  p = a + 2;
  p->data()[3] = 0;
}

void ptr_data_Bad() {
  Block a[2];
  Block* p = a;
  (p + 1)->data()[4] = 0;
}

void ptr_data_loop_Good() {
  Block a[3];
  for (Block* p = a; p != a + 3; ++p) {
    for (int i = 0; i < 4; i++) {
      p->data()[i] = i;
    }
  }
}

void ptr_index_Good() {
  Block a[3];
  Block* p = a + 2;
  (*p)[3] = 0;
}

void ptr_index_Bad() {
  Block a[3];
  Block* p = a + 2;
  (*p)[4] = 0;
}

void array_of_arrays_data_Good() {
  std::array<int, 4> a[3];
  a[1].data()[3] = 0;
}

void array_of_arrays_data_Bad() {
  std::array<int, 4> a[3];
  a[1].data()[4] = 0;
}

void nested_array_data_Good() {
  std::array<std::array<int, 4>, 3> a;
  a[1].data()[3] = 0;
}

void nested_array_index_Good() {
  std::array<std::array<int, 4>, 3> a;
  a[2][3] = 0;
}

// operator[] returns a pointer into the elements of the std::array, so the
// index of the C array it returns counts C arrays.
void FP_array_of_c_arrays_index_Good() {
  std::array<int[4], 3> a;
  a[1][3] = 0;
}

// The size of the elements of a std::vector of std::arrays is unknown.
void FP_vector_of_arrays_data_Good() {
  std::vector<std::array<int, 4>> v(2);
  v[1].data()[3] = 0;
}

struct ArrayField {
  std::array<char, 8> name;
};

void array_field_fgets_Good(ArrayField* s, FILE* f) {
  fgets(s->name.data(), sizeof(s->name), f);
}

void array_field_fgets_Bad(ArrayField* s, FILE* f) {
  fgets(s->name.data(), 9, f);
}

class ArrayMember {
  std::array<int, 4> arr_;

 public:
  void iter_Good() {
    for (auto it = arr_.begin(); it != arr_.end(); ++it) {
      *it = 0;
    }
  }

  void end_Bad() { *arr_.end() = 0; }
};

void array_new_Good() {
  std::array<int, 4>* p = new std::array<int, 4>;
  (*p)[3] = 0;
  p->data()[3] = 0;
  delete p;
}

void array_new_Bad() {
  std::array<int, 4>* p = new std::array<int, 4>;
  memset(p->data(), 0, sizeof(*p) + 1);
  delete p;
}

const std::array<int16_t, 4> global_table = {1, 2, 3, 4};

void global_array_index_Bad() {
  int a[2];
  int i = global_table[1] & 3;
  a[i + 2] = 0;
}

void global_array_data_Bad() {
  int a[2];
  int i = global_table.data()[1] & 3;
  a[i + 2] = 0;
}
