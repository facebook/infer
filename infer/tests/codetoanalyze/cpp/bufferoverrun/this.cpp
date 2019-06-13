/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class CThis {
  unsigned int n;
  void access_Good() {
    char a[this->n + 1];
    a[this->n] = 0;
  }
  void access_Bad() {
    char a[this->n + 1];
    a[this->n + 1] = 0;
  }
};
