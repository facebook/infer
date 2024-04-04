// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace SMRT6;

// testing trait + parent when fn is in a trait

trait T0 {
  public static function f(): int {
    return \Level1\taintSource();
  }

  public static function g(): int {
    return 42;
  }
}

class P0 {
  use T0;
}

trait T {
  require extends P0;

  public static function f_self(): int {
    return parent::f();
  }

  public static function g_self(): int {
    return parent::g();
  }
}

class C0 extends P0 {
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
