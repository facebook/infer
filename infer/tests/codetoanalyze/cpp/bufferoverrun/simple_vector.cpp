/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include <cstdlib>

class int_vector {
  unsigned int _size;
public:
  // FAILS TO RELATE output field _size to argument size, NEED STRONG UPDATE
  int_vector(int size): _size(size) {}
  // FAILS TO SET output field _size to 0
  int_vector(): int_vector(0) {}

  void access_at(int i) {
    int* dummy_array = (int*)malloc(sizeof(int) * _size);
    int dummy_value = dummy_array[i];
    free(dummy_array);
  }
  unsigned int size() {
    return _size;
  }

  void resize(int newsize) {
    _size = newsize;
  }
};

void my_vector_oob_Bad(int_vector& v) {
  unsigned int n = v.size();
  v.access_at(n);
}

// We expect the error to be throw in my_vector_oob_Bad already
void instantiate_my_vector_oob_Ok() {
  int_vector v;
  v.resize(42);
  my_vector_oob_Bad(v);
}
