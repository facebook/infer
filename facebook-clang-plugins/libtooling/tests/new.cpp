/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct pod {
  int a;
};

struct cpplike {
  int a;
  int b;
  pod c;
  cpplike() : a(10), b(2) {}
  cpplike(int a, int b) : a(a), b(b) {}
};

void test() {
  auto *i = new int(2);
  auto *i_a = new int[10];

  auto *p = new pod;
  auto *p_a = new pod[10];

  auto *c = new cpplike(1, 2);
  auto *c_a = new cpplike[10];

  delete i;
  delete[] i_a;
  delete p;
  delete[] p_a;
  delete c;
  delete[] c_a;
}

// This isn't exported quite right yet
void test_c11() {
  auto *i = new int[3]{1, 2, 3};
  auto *c = new cpplike{1, 2};
  auto *c_a = new cpplike[4]{{1, 2}, {3, 4}, {5, 6}}; // initializer list is one
                                                      // too short
}

void *operator new(unsigned long size, void *ptr, void *ptr2) noexcept {
  return ptr2;
};

void test_placement(void *ptr) {
  int i = 1;
  auto *p = new (ptr, &i) pod;
}
