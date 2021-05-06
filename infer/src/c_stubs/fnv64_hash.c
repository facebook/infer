/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <inttypes.h>
#include <stdio.h>

// 64 bits fnv-1a
const uint64_t FNV64_hash_start = 14695981039346656037ULL;
const uint64_t FNV64_prime = 1099511628211ULL;
uint64_t fnv64_hash_impl(const char* s) {
  uint64_t hash = FNV64_hash_start;
  while (*s != 0) {
    hash ^= *s;
    hash *= FNV64_prime;
    s++;
  }
  return hash;
}

CAMLprim value fnv64_hash(value c) {
  const char* c_string = String_val(c);
  uint64_t hashed = fnv64_hash_impl(c_string);
  char str[21];
  snprintf(str, 21, "%" PRIu64, hashed);
  return caml_copy_string(str);
}
