/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int identity(int x) { return x; }

int bar(int x) {
  if (identity(x)) {
    return 1;
  } else {
    return 0;
  }
}

int baz(int x) {

  if (identity(!x)) {
    return 1;
  } else {
    return 0;
  }
}

int neg(int x) { return !x; }
