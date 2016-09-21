/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int div(const int& v) { return 1 / v; }

int div0_init_expr() {
  const int& a = 0;
  return div(a);
}

int div0_function_param_cast() { return div(0); }

// to compare with cfgs when no MaterializeTemporaryExpr is produced
int div0_no_const_ref() {
  int a = 0;
  return div(a);
}
