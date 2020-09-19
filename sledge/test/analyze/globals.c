/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

char* mutable_string = "a string";
const char* immutable_string = "a string";

int mutable_int = 0;
const int immutable_int = 0;

int main() {
  int mi = mutable_int;
  int imi = immutable_int;
  char* c = mutable_string;
  char a_char = *mutable_string;
  char b_char = *immutable_string;

  return 0;
}
