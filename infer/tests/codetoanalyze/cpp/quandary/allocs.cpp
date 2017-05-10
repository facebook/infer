/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdlib.h>
#include <unistd.h>

namespace allocs {

extern int* allocation_source();

void untrusted_malloc_bad() { malloc(*allocation_source()); }

void untrusted_calloc_bad1() { calloc(*allocation_source(), sizeof(int)); }

void untrusted_calloc_bad2() { calloc(5, *allocation_source()); }

void untrusted_reaalloc_bad1() { realloc(allocation_source(), sizeof(int)); }

void untrusted_reaalloc_bad2(int* i) { realloc(i, *allocation_source()); }

void untrusted_brk_bad() { brk((void*)allocation_source()); }

void untrusted_sbrk_bad() { sbrk(*allocation_source()); }
}
