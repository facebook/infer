/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

typedef enum MyOption {
  MyOption1 = 1 << 0,
  MyOption2 = 1 << 1,
};

int main() {
  enum MyOption option1 = MyOption1;
  enum MyOption option2 = MyOption2;
}
