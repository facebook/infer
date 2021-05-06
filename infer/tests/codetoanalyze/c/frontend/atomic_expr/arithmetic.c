/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdatomic.h>

void c11_builtins() {
  int _a;
  atomic_int a;
  atomic_init(&a, 0);
  _a = atomic_fetch_add(&a, 1);
  _a = atomic_fetch_sub(&a, 1);
  _a = atomic_fetch_and(&a, 1);
  _a = atomic_fetch_or(&a, 1);
  _a = atomic_fetch_xor(&a, 1);
}

void opencl_builtins() {
  int _o;
  atomic_int o;
  __opencl_atomic_init(&o, 0);
  _o = __opencl_atomic_fetch_add(
      &o, 1, __ATOMIC_SEQ_CST, __OPENCL_MEMORY_SCOPE_WORK_GROUP);
  _o = __opencl_atomic_fetch_sub(
      &o, 1, __ATOMIC_SEQ_CST, __OPENCL_MEMORY_SCOPE_WORK_GROUP);
  _o = __opencl_atomic_fetch_and(
      &o, 1, __ATOMIC_SEQ_CST, __OPENCL_MEMORY_SCOPE_WORK_GROUP);
  _o = __opencl_atomic_fetch_or(
      &o, 1, __ATOMIC_SEQ_CST, __OPENCL_MEMORY_SCOPE_WORK_GROUP);
  _o = __opencl_atomic_fetch_xor(
      &o, 1, __ATOMIC_SEQ_CST, __OPENCL_MEMORY_SCOPE_WORK_GROUP);
}

void gnu_builtins() {
  int _i;
  int i = 0;
  _i = __atomic_fetch_add(&i, 1, __ATOMIC_SEQ_CST);
  _i = __atomic_fetch_sub(&i, 1, __ATOMIC_SEQ_CST);
  _i = __atomic_fetch_and(&i, 1, __ATOMIC_SEQ_CST);
  _i = __atomic_fetch_or(&i, 1, __ATOMIC_SEQ_CST);
  _i = __atomic_fetch_xor(&i, 1, __ATOMIC_SEQ_CST);
  _i = __atomic_add_fetch(&i, 1, __ATOMIC_SEQ_CST);
  _i = __atomic_sub_fetch(&i, 1, __ATOMIC_SEQ_CST);
  _i = __atomic_and_fetch(&i, 1, __ATOMIC_SEQ_CST);
  _i = __atomic_or_fetch(&i, 1, __ATOMIC_SEQ_CST);
  _i = __atomic_xor_fetch(&i, 1, __ATOMIC_SEQ_CST);
}

void unimplemented() {
  int i;
  atomic_int a;
  __atomic_fetch_max(&i, 0, __ATOMIC_SEQ_CST);
  __atomic_fetch_min(&i, 0, __ATOMIC_SEQ_CST);
  __atomic_fetch_nand(&i, 0, __ATOMIC_SEQ_CST);
  __atomic_max_fetch(&i, 0, __ATOMIC_SEQ_CST);
  __atomic_min_fetch(&i, 0, __ATOMIC_SEQ_CST);
  __atomic_nand_fetch(&i, 0, __ATOMIC_SEQ_CST);
  __c11_atomic_fetch_max(&a, 0, __ATOMIC_SEQ_CST);
  __c11_atomic_fetch_min(&a, 0, __ATOMIC_SEQ_CST);
  __opencl_atomic_fetch_max(
      &a, 0, __ATOMIC_SEQ_CST, __OPENCL_MEMORY_SCOPE_WORK_GROUP);
  __opencl_atomic_fetch_min(
      &a, 0, __ATOMIC_SEQ_CST, __OPENCL_MEMORY_SCOPE_WORK_GROUP);
}
