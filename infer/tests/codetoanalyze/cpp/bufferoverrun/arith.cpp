/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <cstdint>
#include <cstdlib>

void sizeof_bool_Good() {
  int a[2];
  int z = sizeof(bool); // z is 1 (byte)
  a[z] = 0;
}

void sizeof_bool_Bad() {
  int a[1];
  int z = sizeof(bool); // z is 1 (byte)
  a[z] = 0;
}

// FP due to incomplete frontend translation of casting
void range_bool_Good_FP() {
  int a[2];
  bool x = true + true; // x is 1 (true)
  a[x] = 0;
}

void range_bool_Bad() {
  int a[1];
  bool x = true + false; // x is 1 (true)
  a[x] = 0;
}

void bool_overflow_Good_FP() {
  int a[10];
  if (((bool)-1) - 1) {
    a[10] = 0;
  } else {
    a[5] = 0;
  }
}

void bool_overflow_Bad() {
  int a[10];
  if (((bool)-1) - 1) {
    a[10] = 0;
  } else {
    a[15] = 0;
  }
}

void bool_overflow2_Good_FP() {
  int a[10];
  if (((bool)-2) - 1) {
    a[10] = 0;
  } else {
    a[5] = 0;
  }
}

class RG {
 public:
  RG(uint32_t init) {
    seed = init;
    x = init;
  }

  inline uint32_t integer_overflow_rand() {
    uint32_t max = 4294967295;
    return (seed = seed * max);
  }

  inline uint32_t integer_overflow_x() {
    uint32_t max = 4294967295;
    return (x = x * max);
  }

 private:
  uint32_t seed;
  uint32_t x;
};

uint32_t call_integer_overflow_rand_Good() {
  RG generator(4294967295);
  return generator.integer_overflow_rand();
}

uint32_t call_integer_overflow_x_Bad() {
  RG generator(4294967295);
  return generator.integer_overflow_x();
}

struct S_prng_lfsr {
  uint32_t prng_lfsr;
};

void integer_overflow_field_Good(struct S_prng_lfsr* c) {
  c->prng_lfsr = 1;
  c->prng_lfsr = 0 - c->prng_lfsr;
}

struct S_x {
  uint32_t x;
};

void integer_overflow_field_Bad(struct S_x* c) {
  c->x = 1;
  c->x = 0 - c->x;
}

uint32_t integer_overflow_param_1(uint32_t seed) { return seed - 1; }

void call_integer_overflow_param_1_Good() { integer_overflow_param_1(0); }

uint32_t integer_overflow_param_2(uint32_t x) { return x - 1; }

void call_integer_overflow_param_2_Bad() { integer_overflow_param_2(0); }

// "HaSh" (not "hash") is fot checking case-insensitive comparison.
void allow_listed_HaSh_Good() {
  uint32_t x = -1;
  uint32_t y = x * 8;
}

void mod_ub(const char* msg, size_t leng) {
  size_t rem = leng % 32;
  if (rem == 15) {
    char d = msg[14]; // Good
  }
  if (rem == 10) {
    char d = msg[14]; // Bad
  }
}

void call_mod_ub_1_Good() { mod_ub("ab", 2); }

void call_mod_ub_2_Good() { mod_ub("abcdefghijklmno", 15); }

void call_mod_ub_Bad() { mod_ub("abcdefghij", 10); }
