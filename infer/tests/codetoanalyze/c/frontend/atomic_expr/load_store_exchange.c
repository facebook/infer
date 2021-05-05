/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdatomic.h>
#include <stdbool.h>

void c11_builtins() {
  bool b;
  int _a;
  atomic_int a;
  atomic_init(&a, 0);
  _a = atomic_load(&a);
  atomic_store(&a, 1);
  _a = atomic_exchange(&a, 2);
  b = atomic_compare_exchange_strong(&a, &_a, 0);
  b = atomic_compare_exchange_weak(&a, &_a, 0);
}

void opencl_builtins() {
  bool b;
  int _o;
  atomic_int o;
  __opencl_atomic_init(&o, 0);
  _o = __opencl_atomic_load(
      &o, __ATOMIC_SEQ_CST, __OPENCL_MEMORY_SCOPE_WORK_GROUP);
  __opencl_atomic_store(
      &o, 1, __ATOMIC_SEQ_CST, __OPENCL_MEMORY_SCOPE_WORK_GROUP);
  _o = __opencl_atomic_exchange(
      &o, 2, __ATOMIC_SEQ_CST, __OPENCL_MEMORY_SCOPE_WORK_GROUP);
  b = __opencl_atomic_compare_exchange_strong(&o,
                                              &_o,
                                              0,
                                              __ATOMIC_SEQ_CST,
                                              __ATOMIC_SEQ_CST,
                                              __OPENCL_MEMORY_SCOPE_WORK_GROUP);
  b = __opencl_atomic_compare_exchange_weak(&o,
                                            &_o,
                                            0,
                                            __ATOMIC_SEQ_CST,
                                            __ATOMIC_SEQ_CST,
                                            __OPENCL_MEMORY_SCOPE_WORK_GROUP);
}

void gnu_builtins() {
  bool b;
  int _i;
  int _a;
  int i = 0;
  _i = __atomic_load_n(&i, __ATOMIC_SEQ_CST);
  __atomic_load(&i, &_i, __ATOMIC_SEQ_CST);
  __atomic_store_n(&i, 1, __ATOMIC_SEQ_CST);
  __atomic_store(&i, &_i, __ATOMIC_SEQ_CST);
  _i = __atomic_exchange_n(&i, 1, __ATOMIC_SEQ_CST);
  __atomic_exchange(&i, &_i, &_a, __ATOMIC_SEQ_CST);
  b = __atomic_compare_exchange_n(
      &i, &_i, 0, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
  b = __atomic_compare_exchange(
      &i, &_i, &_a, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}
