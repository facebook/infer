/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <map>
#include <string>
#include <vector>

using namespace std;

enum VALUE {
  VALUE1,
  VALUE2,
};

vector<int> global_v = {1, 2, 3};

struct A {
  int a;
  int b;
};

int f(int x) { return x + 1; }

int test(int x) {
  A a{1, 2};
  pair<int, int> p = {3, 4};
  vector<int> v1;
  vector<int> v2{1}; // const
  static vector<int> v3 = {1, 2};
  vector<VALUE> v4 = {VALUE1, VALUE2}; // const
  vector<int> v5 = {f(1), f(2), 3}; // const
  vector<int> v6 = {f(x), 2, 3};

  map<string, int> m1{{"a", 1}, {"b", 2}};

  return a.a + p.first;
}
