/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef struct _ {
  int x;
  int a[30];
} S;

int main() {
  S s;
  s = s;
  return s.x;
}
