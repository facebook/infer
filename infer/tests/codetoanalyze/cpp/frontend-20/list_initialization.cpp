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
