/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct symb_arr_alloc {
  char h[10];
  void symb_arr_access_ok() { h[9] = '\0'; }
  void symb_arr_access_bad() { h[10] = '\0'; }
};
