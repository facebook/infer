// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace SMRT5;

// testing trait + parent where fn is higher in the hierarchy

class P0 {
  public static function f(): int {
    return \Level1\taintSource();
  }

  public static function g(): int {
    return 42;
  }
}

class P1 extends P0 {}
class P2 extends P1 {}

trait T {
  require extends P2;

  public static function f_self(): int {
    return parent::f();
  }

  public static function g_self(): int {
    return parent::g();
  }
}

class C0 extends P2 {
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
