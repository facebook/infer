/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

class SetTest {

  // // Set.of creates an immutable set
  // private static final Set<String> StaticSet = Set.of("a");

  void contains_empty_constant(HashSet<String> set) {
    set.contains("");
  }

  void containsAll_linear(Set<String> set, ArrayList<String> list) {
    set.containsAll(list);
  }

  void removeAll_linear(HashSet<String> set, ArrayList<String> list) {
    set.removeAll(list);
  }

  // void loop_of_constant() {
  //   Set<String> my_set = Set.of("a", "b", "c", "d", "e", "f", "g");
  //   for (String el : my_set) {}
  // }

  // // here, we cannot yet recognize that StaticSet is an immutable set
  // // created by Set.of operation in the class initializer.
  // void immutable_set_of_constant_FP() {
  //   for (int i = 0; i <= StaticSet.size(); i++) {}
  // }
}
