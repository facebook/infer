/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstdint>
#include <cstring>
#include <vector>

namespace stride {

template <typename T>
class Holder {
 public:
  explicit Holder(T* p) : ptr_(p) {}
  T* get() const { return ptr_; }

 private:
  T* ptr_;
};

void memcpy_field_getter_Good(const uint8_t* src) {
  Holder<uint8_t> h(new uint8_t[16]);
  memcpy(h.get(), src, 16);
}

void memcpy_field_getter_Bad(const uint8_t* src) {
  Holder<uint8_t> h(new uint8_t[16]);
  memcpy(h.get(), src, 17);
}

uint8_t read_one_byte_Good(uint8_t v) {
  Holder<uint8_t> h(new uint8_t(v));
  return h.get()[0];
}

uint32_t read_one_byte_as_word_Bad(uint8_t v) {
  Holder<uint8_t> h(new uint8_t(v));
  const uint32_t* w = (const uint32_t*)h.get();
  return w[0];
}

void memset_vector_data_Good() {
  std::vector<uint32_t> v(4);
  memset(v.data(), 0, v.size() * sizeof(uint32_t));
}

void memset_vector_data_Bad() {
  std::vector<uint32_t> v(4);
  memset(v.data(), 0, 20);
}

void memset_vector_init_list_Good() {
  std::vector<uint32_t> v{1, 2, 3, 4};
  memset(v.data(), 0, 16);
}

void memset_vector_init_list_Bad() {
  std::vector<uint32_t> v{1, 2, 3, 4};
  memset(v.data(), 0, 20);
}

void memset_vector_sizeof_Good() {
  std::vector<uint8_t> v(4 * sizeof(uint32_t));
  memset(v.data(), 0, 16);
}

void memset_vector_sizeof_Bad() {
  std::vector<uint8_t> v(4 * sizeof(uint32_t));
  memset(v.data(), 0, 17);
}

void memset_vector_doubles_Good() {
  std::vector<double> v(4);
  memset(v.data(), 0, 4 * sizeof(double));
}

void memset_vector_doubles_Bad() {
  std::vector<double> v(4);
  memset(v.data(), 0, 5 * sizeof(double));
}

void clear_bytes(uint8_t* p, size_t n) { memset(p, 0, n); }

void clear_vector_as_bytes_Good() {
  std::vector<uint32_t> v(4);
  clear_bytes((uint8_t*)v.data(), v.size() * sizeof(uint32_t));
}

void clear_vector_as_bytes_Bad() {
  std::vector<uint32_t> v(4);
  clear_bytes((uint8_t*)v.data(), 20);
}

void fill(void* p, size_t n) { memset(p, 0, n); }

void fill_vector_floats_Good() {
  std::vector<float> v(4);
  fill(v.data(), v.size() * sizeof(float));
}

void fill_vector_floats_Bad() {
  std::vector<float> v(4);
  fill(v.data(), 5 * sizeof(float));
}

void copy_utf16(char16_t* dst, const char16_t* src, size_t n) {
  memcpy(dst, src, n * sizeof(char16_t));
}

void copy_utf16_Good() {
  char16_t dst[8];
  char16_t src[8] = {};
  copy_utf16(dst, src, 8);
}

void copy_utf16_Bad() {
  char16_t dst[8];
  char16_t src[16] = {};
  copy_utf16(dst, src, 9);
}

void clear_wchars(wchar_t* p, size_t n) { memset(p, 0, n * sizeof(wchar_t)); }

void clear_wchars_Good() {
  wchar_t a[4];
  clear_wchars(a, 4);
}

void clear_wchars_Bad() {
  wchar_t a[4];
  clear_wchars(a, 5);
}

enum class Wide : uint64_t { A, B };

void clear_wides(Wide* p, size_t n) { memset(p, 0, n * sizeof(Wide)); }

// The frontend translates every enum type to int, so the elements of p seem to
// be 4 bytes long.
void FP_clear_wides_Good() {
  Wide a[4];
  clear_wides(a, 4);
}

} // namespace stride
