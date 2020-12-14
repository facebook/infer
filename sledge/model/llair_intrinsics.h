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

/* allocation that cannot fail */
void* __llair_alloc(unsigned size);

/* non-deterministic choice */
int __llair_choice();

/* throw an exception */
__attribute__((noreturn)) void __llair_throw(void* thrown_exception);

/* executions that call __llair_unreachable are assumed to be impossible */
__attribute__((noreturn)) void __llair_unreachable();

#ifdef __cplusplus
}
#endif
