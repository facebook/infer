/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class Valid {
  public static boolean is_valid_vn(int i) {
    return i >= 1 && i <= 10;
  }

  public static void id_to_vn(int i) {}

  public static void safe_call(int id) {
    if (is_valid_vn(id)) {
      id_to_vn(id);
    }
  }

  public static void FP_testOk() {
    safe_call(1);
  }
}
