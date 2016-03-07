/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

struct MyHasher {
  static int hash(int t) { return 1; }
};

template <class Hasher, typename T, typename... Ts>
int hash_combine_generic(const T& t, const Ts&... ts) {
  int seed = Hasher::hash(t);
  if (sizeof...(ts) == 0) {
    return seed;
  }
  return 0;
}

int test = hash_combine_generic<MyHasher>(0, 0, 0);
