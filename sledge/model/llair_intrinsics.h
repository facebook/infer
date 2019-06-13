/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

__attribute__((noreturn)) void __llair_throw(void* thrown_exception);

/* This models allocation that cannot fail. */
void* __llair_alloc(unsigned size);

#ifdef __cplusplus
}
#endif
