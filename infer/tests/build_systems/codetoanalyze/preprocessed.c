/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
# 1 "/tmp/removed_src.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 330 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/tmp/removed_src.c" 2

# 1 "/tmp/removed_header.h" 1

void fun();
# 3 "/tmp/removed_src.c" 2

int deref(int* a) { return *a; }

int test() { return deref(0); }
