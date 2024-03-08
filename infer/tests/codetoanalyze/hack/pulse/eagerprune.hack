// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// These tests are all OK, but useful for looking at the debug output to confirm
// that the eager pruning is working as expected
class EagerTypePrune {
  public static function testPositiveOK(): void {
    $i = 3;
    if ($i is int) {
      return;
    } else {
      // if we're really eager, we shouldn't even analyse this call
      // have confirmed that we do at the moment
      $dummy = self::g();
    }
  }

  public static function testNegativeOK(): void {
    $i = 3;
    if ($i is string) {
      $dummy = self::g();
    } else {
      return;
    }
  }

  public static function testBothOK(mixed $i): void {
    if ($i is int) {
      $dummy = self::g();
    } else {
      $dummy = self::f();
    }
  }

  public static function g(): int {
    return 42;
  }

  public static function f(): int {
    return 42;
  }
}
