/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class StringBuilderTest {

  StringBuilder mId;

  void new_linear(String s) {
    String str = new StringBuilder(s).toString();
    for (int i = 0; i < str.length(); i++) {}
  }

  void new_constant() {
    String s = new StringBuilder("hello").toString();
    new_linear(s);
  }

  void new_capacity_constant() {
    String s =
        new StringBuilder(10).toString(); // capacity is irrelevant to underlying size of the string
    new_linear(s);
  }

  void append_linear(String s) {
    String str = new StringBuilder(s).append("me").toString();
    new_linear(str);
  }
}
