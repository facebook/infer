/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

void stat_cast() {
  int a;
  long long la = static_cast<long long>(a);
}

void functional_cast() { int a = int(2 + 3.4); }
