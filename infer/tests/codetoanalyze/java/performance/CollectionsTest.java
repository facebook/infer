/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

class CollectionsTest {

  int binary_search_log(List<String> list) {
    return Collections.binarySearch(list, "x");
  }

  void shuffle_linear(List<String> list, Random mRandom) {
    Collections.shuffle(list, mRandom);
    for (int i = 0; i < list.size(); i++) {}
  }

  void singletonSet_constant() {
    Set<String> set = Collections.singleton("ezgi");
    for (String s : set) {}
  }

  void singletonList_constant(String el) {
    List<String> list = Collections.singletonList(el);
    for (String s : list) {}
  }

  void fill_linear(List<String> list, String el) {
    Collections.fill(list, el);
  }

  void reverse_linear(List<String> list) {
    Collections.reverse(list);
  }

  void reverse_constant(String el) {
    List<String> list = Collections.singletonList(el);
    Collections.reverse(list);
  }

  void copy_linear(List<String> list_from, List<String> list_to) {
    Collections.copy(list_to, list_from);
  }

  void unmodifiable_linear(List<String> list) {
    List<String> unmod_list = Collections.unmodifiableList(list);
    for (int i = 0; i < unmod_list.size(); i++) {}
  }

  void unmodifiable_map(Map<String, String> map) {
    for (Map.Entry<String, String> entry : Collections.unmodifiableMap(map).entrySet()) {}
  }

  void unmodifiable_set(Set<Integer> set) {
    for (Integer el : Collections.unmodifiableSet(set)) {}
  }

  void emptySet_constant() {
    Set<String> set = Collections.emptySet();
    for (String s : set) {}
  }

  void emptyList_constant() {
    for (int i = 0; i < Collections.emptyList().size(); i++) {}
  }

  void singletonMap_constant() {
    for (int i = 0; i < Collections.singletonMap(1, 1).size(); i++) {}
  }

  void globalEmptyList_constant() {
    for (int i = 0; i < java.util.Collections.EMPTY_LIST.size(); i++) {}
  }
}
