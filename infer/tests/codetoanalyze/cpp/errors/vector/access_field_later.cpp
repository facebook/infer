/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <vector>

// has copy constructor
struct WithCopy {
  int f1;
  int f2;
};

// no copy constructor - important to catch wrong implementations of std::vector
// model
struct WithoutCopy {
  WithoutCopy() = default;
  WithoutCopy(WithoutCopy&& x) = default;
  WithoutCopy(const WithoutCopy& x) = delete;
  WithoutCopy& operator=(const WithoutCopy&) = delete;
  int f1;
  int f2;
};

int getIntPtrOk(int id, std::vector<int*>& v) {
  int x = v.size();
  int** res = &v[id];
  return **res;
}

int getWithCopyOk(int id, std::vector<WithCopy>& v) {
  int x = v.size();
  WithCopy* res = &v[id];
  return res->f1;
}

int getWithCopyPtrOk(int id, std::vector<WithCopy*>& v) {
  int x = v.size();
  WithCopy** res = &v[id];
  return (*res)->f1;
}

int getWithoutCopyOk(int id, std::vector<WithoutCopy>& v) {
  int x = v.size();
  WithoutCopy* res = &v[id];
  return res->f1;
}

int getWithoutCopyPtrOk(int id, std::vector<WithoutCopy*>& v) {
  int x = v.size();
  WithoutCopy** res = &v[id];
  return (*res)->f1;
}
