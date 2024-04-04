// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace FloatTests;

class Main {

  // generates $builtins.hhbc_is_type_dbl
  public static function is_test(mixed $arg): float {
    if ($arg is float) {
      return $arg;
    } else {
      return 0.0;
    }
  }
}

class Tests {

  public static function is_v1_bad(): void {
    $val = 3.14;
    if (Main::is_test($val) == $val) {
      \Level1\taintSink(\Level1\taintSource());
    }
  }

  public static function is_v2_bad(): void {
    if (Main::is_test("") == 0.0) {
      \Level1\taintSink(\Level1\taintSource());
    }
  }

  public static function is_v1_ok(): void {
    $val = 3.14;
    if (Main::is_test($val) != $val) {
      \Level1\taintSink(\Level1\taintSource());
    }
  }

  public static function is_v2_ok(): void {
    if (Main::is_test("") != 0.0) {
      \Level1\taintSink(\Level1\taintSource());
    }
  }
}
