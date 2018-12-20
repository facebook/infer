/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.bufferoverrun;

import java.util.ArrayList;

class Array {
  private ArrayList a = new ArrayList<>();

  void collection_add_zero_Good() {
    a.add(0, 100);
  }

  ArrayList collection_remove_from_empty_Bad() {
    ArrayList b = new ArrayList<>();
    b.remove(0);
    return b;
  }
}
