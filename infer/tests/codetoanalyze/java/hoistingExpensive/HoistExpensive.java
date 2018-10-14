/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class HoistExpensive {

  int incr(int x) {
    return x + 1;
  }

  // incr will not be hoisted since it is cheap
  void cheap_dont_hoist(int size) {
    int x = 10;
    for (int i = 0; i < size; i++) {
      incr(x);
    }
  }

  // call to cheap_dont_hoist will be hoisted since it is expensive.
  void symbolic_expensive_hoist(int size) {
    for (int i = 0; i < size; i++) {
      cheap_dont_hoist(size);
    }
  }

  // call to cheap_dont_hoist will NOT be hoisted since it is cheap.
  void instantiated_cheap_hoist(int size) {
    for (int i = 0; i < size; i++) {
      cheap_dont_hoist(1);
    }
  }
}
