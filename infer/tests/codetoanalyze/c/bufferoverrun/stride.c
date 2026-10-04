/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void memset_bytes(uint8_t* p, size_t n) { memset(p, 0, n); }

void memset_bytes_param_Good() {
  uint8_t a[16];
  memset_bytes(a, 16);
}

void memset_bytes_param_Bad() {
  uint8_t a[16];
  memset_bytes(a, 17);
}

void memset_words(uint32_t* p, size_t n) { memset(p, 0, n); }

void memset_words_param_Good() {
  uint32_t a[4];
  memset_words(a, 16);
}

void memset_words_param_Bad() {
  uint32_t a[4];
  memset_words(a, 20);
}

// The pointer to the first element of a two-dimensional array only knows the
// size of the first row.
void FP_memset_words_matrix_Good() {
  uint32_t a[4][4];
  memset_words(&a[0][0], sizeof(a));
}

void clear_tail(uint16_t* p, size_t from, size_t n) {
  memset(p + from, 0, (n - from) * sizeof(uint16_t));
}

void clear_tail_Good() {
  uint16_t a[8];
  clear_tail(a, 2, 8);
}

void clear_tail_Bad() {
  uint16_t a[8];
  clear_tail(a, 2, 9);
}

void memcpy_chars(char* dst, const char* src, size_t n) { memcpy(dst, src, n); }

void memcpy_chars_param_Good(const char* src) {
  char a[16];
  memcpy_chars(a, src, 16);
}

void memcpy_chars_param_Bad(const char* src) {
  char a[16];
  memcpy_chars(a, src, 17);
}

void memcpy_chars_param_heap_Bad(const char* src) {
  char* p = malloc(16);
  if (p) {
    memcpy_chars(p, src, 17);
    free(p);
  }
}

void copy_name(char* dst, const char* src) { strncpy(dst, src, 16); }

void copy_name_literal_Good() {
  char name[16];
  copy_name(name, "eth0");
}

void copy_name_literal_Bad() {
  char name[8];
  copy_name(name, "eth0");
}

void copy_name_param_Good(const char* src) {
  char name[16];
  copy_name(name, src);
}

void copy_name_unterminated_Bad() {
  char src[8];
  memset(src, 'a', sizeof(src));
  char name[16];
  copy_name(name, src);
}

void read_line(char* buf, int n, FILE* f) { fgets(buf, n, f); }

void fgets_param_Good(FILE* f) {
  char a[16];
  read_line(a, 16, f);
}

void fgets_param_Bad(FILE* f) {
  char a[16];
  read_line(a, 17, f);
}

struct buffer {
  char* data;
};

void memset_field(struct buffer* b, size_t n) { memset(b->data, 0, n); }

void memset_field_Good() {
  char a[16];
  struct buffer b = {a};
  memset_field(&b, 16);
}

void memset_field_Bad() {
  char a[16];
  struct buffer b = {a};
  memset_field(&b, 17);
}

uint8_t get_byte_of_chars(const char* p, int i) {
  return ((const uint8_t*)p)[i];
}

uint8_t cast_chars_param_Good() {
  char a[16] = {0};
  return get_byte_of_chars(a, 15);
}

uint8_t cast_chars_param_Bad() {
  char a[16] = {0};
  return get_byte_of_chars(a, 16);
}

uint8_t get_byte_of_words(const uint32_t* p, int i) {
  return ((const uint8_t*)p)[i];
}

uint8_t cast_words_param_Good() {
  uint32_t a[4] = {0};
  return get_byte_of_words(a, 15);
}

uint8_t cast_words_param_Bad() {
  uint32_t a[4] = {0};
  return get_byte_of_words(a, 16);
}

uint32_t get_word_of_chars(const char* p, int i) {
  return ((const uint32_t*)p)[i];
}

uint32_t cast_chars_to_words_param_Good() {
  char a[16] = {0};
  return get_word_of_chars(a, 3);
}

uint32_t cast_chars_to_words_param_Bad() {
  char a[16] = {0};
  return get_word_of_chars(a, 4);
}

uint8_t get_byte_of_floats(const float* p, int i) {
  return ((const uint8_t*)p)[i];
}

uint8_t cast_floats_param_Good() {
  float a[4] = {0};
  return get_byte_of_floats(a, 15);
}

uint8_t cast_floats_param_Bad() {
  float a[4] = {0};
  return get_byte_of_floats(a, 16);
}

void memset_floats_as_bytes(float* p) {
  memset_bytes((uint8_t*)p, 4 * sizeof(float));
}

void memset_floats_as_bytes_Good() {
  float a[4];
  memset_floats_as_bytes(a);
}

void memset_floats_as_bytes_Bad() {
  float a[3];
  memset_floats_as_bytes(a);
}

void memset_words_offset_Good() {
  uint32_t a[4];
  memset(a + 2, 0, 8);
}

void memset_words_offset_Bad() {
  uint32_t a[4];
  memset(a + 2, 0, 12);
}
