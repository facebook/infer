/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdatomic.h>
#include <stdbool.h>

int other_func(int a, int* b);

void arithmetic() {
  int _a;
  int* p = &_a;
  atomic_int a;
  atomic_int a2;
  atomic_init(&a, 0);
  _a = atomic_fetch_add(&a, _a << 2);
  _a = atomic_fetch_sub(&a, atomic_fetch_add(&a2, a ? 5 : _a));
  _a = __opencl_atomic_fetch_add(&a,
                                 1,
                                 a ? __atomic_sub_fetch(p, 5, 0) : 10,
                                 __OPENCL_MEMORY_SCOPE_WORK_GROUP);
  _a = *__atomic_fetch_add(
      &p, __atomic_or_fetch(&_a, _a = 0, __ATOMIC_SEQ_CST), 1);
  _a = __opencl_atomic_fetch_or(&a,
                                other_func(0, NULL),
                                __ATOMIC_SEQ_CST,
                                __OPENCL_MEMORY_SCOPE_WORK_GROUP);
  _a = other_func(atomic_fetch_xor(&a, 1) + 5, 0);
}

void load_store_exchange() {
  bool b;
  int** p;
  int _a;
  atomic_int a;
  atomic_int a2;
  atomic_init(&a, 1 + _a);
  _a = (atomic_load(&a) ? 5 : 2);
  _a = atomic_load(b ? &a : &a2);
  __opencl_atomic_store(
      b ? &a : &a2, 1, _a++ - 20, __OPENCL_MEMORY_SCOPE_WORK_GROUP);
  _a = atomic_exchange(&a, 2);
  other_func(atomic_exchange(&a2, 2), 0);
  other_func(__atomic_compare_exchange_n(
                 *p, &_a, **p, true, _a + **p, other_func(0, NULL)),
             0);
  __atomic_compare_exchange_n(&_a,
                              *p,
                              other_func(0, NULL),
                              ++_a | 8,
                              other_func(0, NULL),
                              other_func(atomic_load(&a), NULL));
}
