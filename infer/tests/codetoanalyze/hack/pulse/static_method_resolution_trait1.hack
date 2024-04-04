// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace SMRT1;

// testing trait + self (fn is in the trait)

trait T {
  public static function f(): int {
    return \Level1\taintSource();
  }

  public static function f_self(): int {
    return self::f();
  }

  public static function g(): int {
    return 42;
  }

  public static function g_self(): int {
    return self::g();
  }
}

class C0 {
  use T;
}

function Bad0(): void {
  $tainted = C0::f_self();
  \Level1\taintSink($tainted);
}

function Good0(): void {
  $ok = C0::g_self();
  \Level1\taintSink($ok);
}
