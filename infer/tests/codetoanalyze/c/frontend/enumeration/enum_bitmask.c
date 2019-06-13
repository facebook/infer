/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef enum MyOption {
  MyOption1 = 1 << 0,
  MyOption2 = 1 << 1,
};

int main() {
  enum MyOption option1 = MyOption1;
  enum MyOption option2 = MyOption2;
}
