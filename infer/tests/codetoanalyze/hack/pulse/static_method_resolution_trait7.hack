// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace SMRT7;

// testing trait + optional arguments

trait T {
  public static function with_optional_arguments(int $x, int $y = 0): int {
    return $x + $y;
  }
}

class C0 {
  use T;
}

function Good0(): void {
  if (
    C0::with_optional_arguments(42) != 42
  ) { // calls SMRT7::T$static.with_optional_arguments#3
    $tainted = \Level1\taintSource();
    \Level1\taintSink($tainted);
  }
}

function Good1(): void {
  if (C0::with_optional_arguments(42, -42) != 0) {
    $tainted = \Level1\taintSource();
    \Level1\taintSink($tainted);
  }
}

function Bad0(): void {
  if (C0::with_optional_arguments(42) == 42) {
    $tainted = \Level1\taintSource();
    \Level1\taintSink($tainted);
  }
}

function Bad1(): void {
  if (C0::with_optional_arguments(42, -42) == 0) {
    $tainted = \Level1\taintSource();
    \Level1\taintSink($tainted);
  }
}
