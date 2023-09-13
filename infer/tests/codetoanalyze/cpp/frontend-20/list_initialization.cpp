/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct SuperClass {
  int super_f1;
  int super_f2;
};

struct CurrentClass : public SuperClass {
  int cur_f1;
  int cur_f2;
  int cur_f3;
};

void init_all() { CurrentClass x{42, 52, 62, 72, 82}; }

void init_some() { CurrentClass x{42, 52, 62}; }

void init_with_field_name() { CurrentClass x{.cur_f1 = 42, .cur_f3 = 62}; }

#include "list_initialization.h"

// When the super class is declared in a deep header, it may not be able to
// translate the list-initialization correctly.
void init_class_in_header() { ClassInHeader x{42, 52}; }

struct CurrentClass2 : SuperClass {
  CurrentClass2();
};

CurrentClass2::CurrentClass2() : SuperClass{.super_f1 = 42, .super_f2 = 52} {}
