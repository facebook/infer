/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class StringTest {
  void constant_Good() {
    String s = "hello";
    char c = s.charAt(4);
  }

  void constant_Bad() {
    String s = "hello";
    char c = s.charAt(5);
  }

  void constant_explicit_constructor_Good() {
    String s = new String("hello");
    char c = s.charAt(4);
  }

  void constant_explicit_constructor_Bad() {
    String s = new String("hello");
    char c = s.charAt(5);
  }

  void copy_constructor_Good() {
    String s = new String("hello");
    String t = new String(s);
    char c = t.charAt(4);
  }

  void copy_constructor_Bad() {
    String s = new String("hello");
    String t = new String(s);
    char c = t.charAt(5);
  }
}
