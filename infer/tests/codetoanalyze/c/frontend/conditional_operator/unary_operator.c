/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void dereference_ifthenelse(int* p) {
  int x;
  x = *(1 ? p : p);

  int y = *(1 ? p : p);

  *(1 ? p : p);
}
