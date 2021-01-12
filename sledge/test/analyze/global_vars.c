/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const char* a_string = "I'm a string";
int an_int = 0;

int idx() { return an_int; }

int main() {
  const char* str = a_string;

  return str[idx()];
}
