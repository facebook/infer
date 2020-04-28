/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

void wrap_free(void* x) { free(x); }

void interproc_free_ok() {
  int* p = malloc(sizeof(int));
  wrap_free(p);
}

void leak_bad() { int* p = malloc(sizeof(int)); }
