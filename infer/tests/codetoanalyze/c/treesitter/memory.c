/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void* malloc(int size);
void free(void* ptr);

void use_after_free_Bad() {
  int* p = malloc(4);
  free(p);
  *p = 42;
}

void no_use_after_free_Ok() {
  int* p = malloc(4);
  if (p != 0) {
    *p = 42;
    free(p);
  }
}
