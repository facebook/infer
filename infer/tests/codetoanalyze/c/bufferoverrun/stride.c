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

int next_tag(void);
void write_unknown(char* p);
char* unknown_buffer(void);

struct slice {
  const char* data;
  size_t size;
};

struct slice make_slice(const char* s) {
  struct slice r = {s, strlen(s)};
  return r;
}

void prefix_message(const struct slice* msg) {
  size_t size = 4 + (msg->size ? 2 + msg->size : 0);
  char* result = malloc(size);
  if (result && msg->size) {
    memcpy(result, "file", 4);
    result[4] = ':';
    result[5] = ' ';
  }
  free(result);
}

void report_tag() {
  const char* msg = NULL;
  int tag;
  while (msg == NULL && (tag = next_tag())) {
    if (tag == 1) {
      msg = "bad tag";
    } else if (tag == 2) {
      msg = "bad length";
    }
  }
  if (msg != NULL) {
    struct slice s = make_slice(msg);
    prefix_message(&s);
  }
}

// Unknown string lengths only give BUFFER_OVERRUN_U5.
void call_write_unknown_Good() {
  write_unknown(unknown_buffer());
  report_tag();
}

void fill(char* p) { write_unknown(p); }

void call_fill_unknown_Good() {
  fill(unknown_buffer());
  report_tag();
}

struct log_buffer {
  const char* data;
  size_t size;
  int checked;
};

uint32_t load32(const char* p) {
  const uint8_t* b = (const uint8_t*)p;
  return b[0] | b[1] << 8 | b[2] << 16 | (uint32_t)b[3] << 24;
}

uint32_t header_checksum(struct log_buffer* b) {
  if (b->size < 7) {
    return 0;
  }
  if (b->checked) {
    return load32(b->data);
  }
  return 1;
}

// The callee condition keeps only the latest prune, [b->checked], not [b->size
// >= 7].
uint32_t FP_header_checksum_empty_Good() {
  struct log_buffer b = {"", 0, 1};
  return header_checksum(&b);
}

uint32_t sum_pairs(const char* data, size_t n) {
  const uint8_t* p = (const uint8_t*)data;
  const uint8_t* e = p + n;
  uint32_t s = 0;
  while (e - p >= 8) {
    s ^= p[7];
    p += 8;
  }
  return s;
}

// The guard on [e - p] does not bound the offset of [p].
uint32_t FP_sum_pairs_short_Good() {
  char a[5] = {0};
  return sum_pairs(a, 1);
}

struct entity {
  const char* pattern;
  size_t length;
};

static const struct entity entities[] = {{"quot", 4}, {"lt", 2}};

void write_chars(char* dst, const char* src, size_t n) { memcpy(dst, src, n); }

// The patterns and lengths of the entities are joined without a relation.
void FP_write_entity_Good(int i) {
  char dst[8];
  write_chars(dst, entities[i].pattern, entities[i].length);
}
