/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void integral_cast() {
  int a;
  int b = ((char)a) + 2;
}

struct object {
  int field;
};

void pointer_cast() {
  void* obj;
  int f = ((struct object*)obj)->field;
}
