/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct X {
  ~X() {}
};

void deleteX(X* x) { delete x; }

void deleteInt(int* x) { delete x; }
