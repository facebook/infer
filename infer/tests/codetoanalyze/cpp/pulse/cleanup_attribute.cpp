/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace cleanup_attribute {

struct Owner {
  int* p;
  ~Owner() { delete p; }
};

void release_owner(Owner* o) {
  delete o->p;
  o->p = nullptr;
}

void delete_owner_field(Owner* o) { delete o->p; }

void cleanup_runs_before_destructor_ok() {
  __attribute__((cleanup(release_owner))) Owner o{new int(42)};
}

void cleanup_and_destructor_double_delete_bad() {
  __attribute__((cleanup(delete_owner_field))) Owner o{new int(42)};
}

void delete_int(int** x) { delete *x; }

void cleanup_delete_ok() {
  __attribute__((cleanup(delete_int))) int* x = new int(42);
}

void cleanup_use_after_delete_bad() {
  int* y;
  {
    __attribute__((cleanup(delete_int))) int* x = new int(42);
    y = x;
  }
  *y = 0;
}

} // namespace cleanup_attribute
