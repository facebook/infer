/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int main() {
  int c = __llair_choice() % 5;
  int* p;

  if (c) {
    p = (int*)malloc(sizeof(int));
  }
  for (int i = 0; i < 4; i++) {
    c += (__llair_choice() % 3);
  }
  if (c) {
    *p = 0;
    free(p);
  }
  return 0;
}
