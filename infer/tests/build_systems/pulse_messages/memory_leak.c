/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void intraprocedural_leak_bad() { int* x = (int*)malloc(sizeof(int)); }

int* malloc_wrapper() { return (int*)malloc(sizeof(int)); }

void malloc_wrapper_leak_bad() { int* x = malloc_wrapper(); }
