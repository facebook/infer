/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class StringTest {

  String mId;

  int indexof_linear(String m) {
    return m.indexOf('_');
  }

  int indexof_from_linear(String m, int j) {
    return m.indexOf('_', j);
  }

  int indexof_quadratic(String m, String n) {
    return m.indexOf(n);
  }

  int indexof_constant(String n) {
    String m = "hi";
    return m.indexOf('i');
  }

  public String index_substring_linear() {
    int index = indexof_linear(mId);
    return mId.substring(0, index);
  }

  private String startsWith_constant() {
    String s = "";
    return s.startsWith(",") ? s.substring(1) : s;
  }

  void split_linear(String s) {
    String[] list = s.split(",");
    for (int i = 0; i < list.length; i++) {}
  }

  void split_with_limit_linear(String s, int limit) {
    String[] list = s.split(",", limit);
    for (int i = 0; i < list.length; i++) {}
  }

  void call_split_constant() {
    String s = new String("hello");
    split_linear(s);
  }

  void byte_array_constructor_linear(byte[] data) {
    String s = new String(data);
    for (int i = 0; i < s.length(); i++) {}
  }

  void call_string_constant() {
    byte[] data = new byte[10];
    byte_array_constructor_linear(data);
  }

  void string_constructor_constant() {
    String s = "abcd";
    String str = new String(s);
    for (int i = 0; i < str.length(); i++) {}
  }

  void string_constructor_linear(String s) {
    String str = new String(s);
    for (int i = 0; i < str.length(); i++) {}
  }

  void call_string_constructor_constant() {
    String s = new String();
    string_constructor_linear(s);
  }

  public void substring_no_end_linear(String s, int x) {
    String sub = s.substring(x);
    for (int i = 0; i < sub.length(); i++) {}
  }

  public void substring_linear(String s, int x, int y) {
    String sub = s.substring(x, y);
    for (int i = 0; i < sub.length(); i++) {}
  }

  public void replace_linear(String s) {
    String r = s.replace('.', '/');
    for (int i = 0; i < r.length(); i++) {}
  }

  public void last_index_of_linear(String s) {
    int j = s.lastIndexOf('/');
    for (int i = 0; i < j; i++) {}
  }

  boolean unknown_bool;

  public void last_index_of_linear_FN(String s) {
    int i = s.lastIndexOf('/');
    while (i > 0) {
      int j = s.lastIndexOf('/', i - 1);
      if (j > 0) {
        break;
      }
      i = j;
    }
  }

  void class_get_canonical_name_constant(Integer a) {
    for (int i = 0; i < a.getClass().getCanonicalName().length(); i++) {}
  }

  void string_valueOf_linear(char[] input) {
    String s = String.valueOf(input);
    for (int i = 0; i < s.length(); i++) {}
  }

  void string_valueOf_constant() {
    char[] input = {'a', 'b', 'c', 'd', 'e', 'f', 'g'};
    String s = String.valueOf(input);
    for (int i = 0; i < s.length(); i++) {}
  }
}
