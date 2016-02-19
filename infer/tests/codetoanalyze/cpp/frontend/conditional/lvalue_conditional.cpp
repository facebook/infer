/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int choose_lvalue(int a) {
  int v1 = 0, v2 = 1;
  int v3 = a ? v1 : v2;
  return v3;
}

int choose_rvalue(int a) {
  int v1 = 0;
  int v3 = a ? v1 : 1;
  return v3;
}

int assign_conditional(int a) {
  int v1 = 0, v2 = 0;
  (a ? v1 : v2) = 1;
  return v1;
}

int div_temp_lvalue(int a, int b) {
  const int& r = a ? b : 1;
  return 1 / r;
}

int div0_choose_lvalue() { return 1 / choose_lvalue(1); }

int div1_choose_lvalue() { return 1 / choose_lvalue(0); }

int div0_choose_rvalue() { return 1 / choose_rvalue(1); }

int div1_choose_rvalue() { return 1 / choose_rvalue(0); }

int div0_assign_conditional() { return 1 / assign_conditional(0); }

int div1_assign_conditional() { return 1 / assign_conditional(1); }

int div0_temp_lvalue() { return div_temp_lvalue(1, 0); }

int div1_temp_lvalue() { return div_temp_lvalue(0, 1); }
