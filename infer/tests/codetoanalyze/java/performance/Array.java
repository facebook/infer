/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.performance;

import java.util.Arrays;

public class Array {

  public void array_access_good() {
    float[] radii = new float[8];
    for (int i = 0; i < 4; ++i) {
      radii[i * 2] = radii[i];
      radii[i * 2 + 1] = radii[i] + 1;
    }
  }

  public void array_access_overrun_bad() {
    float[] radii = new float[8];
    for (int i = 0; i < 4; ++i) {
      radii[i * 2] = radii[i];
      radii[i * 2 + 2] = radii[i] + 1;
    }
  }

  void array_access_weird_ok(long[] optionNumerators, int length) {
    for (int j = 0; j < length; ++j) {
      if (10 < optionNumerators[j] + 1) {}
    }
  }

  int binary_search_log(String[] arr) {
    return Arrays.binarySearch(arr, "x");
  }

  void fill_linear(String[] arr) {
    Arrays.fill(arr, "x");
  }

  void copyOf_linear(String[] arr) {
    String[] new_arr = Arrays.copyOf(arr, arr.length);
    for (String el : new_arr) {}
  }

  void copyOf_constant(String[] arr) {
    String[] new_arr = Arrays.copyOf(arr, 10);
  }

  void init_array_linear() {
    int[] table = new int[256];
    for (int i = 0; i < table.length; ++i) {
      table[i] = i;
    }
  }

  void fill_big_constant() {
    String[] arr = new String[300];
    Arrays.fill(arr, 0);
  }

  void sort_array_nlogn(int size) {
    String[] arr = new String[size];
    Arrays.sort(arr);
  }

  void bsearch_log(int i) {
    String[] arr = new String[i];
    Arrays.binarySearch(arr, "u");
  }
}
