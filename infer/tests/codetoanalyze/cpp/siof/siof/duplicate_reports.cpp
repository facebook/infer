/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include "siof_types.h"

extern SomeNonPODObject rick;
extern SomeNonPODObject dangerous;

void access_rick() { rick.some_method(); }
void nested_access() {
  access_rick();
  dangerous.some_method();
  access_rick();
}
void nested_nested_access() {
  access_rick();
  nested_access();
  rick.some_method();
}

struct X {
  X() {
    access_rick();
    rick.some_method();
    nested_access();
    nested_access();
    dangerous.some_method();
    nested_nested_access();
  }
};

X many_paths_to_siof_bad;
