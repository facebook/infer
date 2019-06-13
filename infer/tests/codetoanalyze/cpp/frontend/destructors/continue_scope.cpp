/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace continue_scope {

struct X {
  ~X() {}
};

struct vec;

// iterator
struct iterator {
  int position;
  const vec* vector;

  iterator(const vec* v, int pos) : position(pos), vector(v) {}

  iterator operator++() {
    position++;
    return *this;
  }

  bool operator!=(const iterator& i2) { return position != i2.position; }

  X operator*() const;
};

struct vec {
  vec() {}
  iterator begin() { return iterator(this, 0); }
  iterator end() { return iterator(this, 10); }

  X get(int pos) const { return _data[pos]; }

  X _data[10];
};

X iterator::operator*() const { return vector->get(position); }

void test_for_range(bool b) {
  vec vector;
  X x1;
  for (X x : vector) {
    if (b) {
      X x2 = x;
      continue;
    }
  }
}

void test_for(bool b) {
  vec vector;
  for (iterator it = vector.begin(); it != vector.end(); ++it) {
    if (b) {
      X x1;
      continue;
    }
  }
  X x2;
}

void test_while1(bool a, bool b) {
  X x1;
  while (a) {
    if (b) {
      X x2;
      continue;
    } else {
      X x4;
    }
  }
}

void test_do_while(bool a, bool b) {
  X x1;
  do {
    X x2;
    if (b) {
      X x3;
      continue;
    } else {
      X x4;
    }
  } while (a);
}

void test_while2(bool a, bool b) {
  X x1;
  while (a) {
    X x2;
    while (b) {
      X x3;
      continue;
    }
  }
}

void test_while3(bool a, bool b) {
  X x1;
  while (a) {
    X x2;
    while (b) {
      continue;
    }
  }
  X x3;
}

} // namespace continue_scope
