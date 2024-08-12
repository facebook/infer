// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace TypeConstant;

final class HasTCInt {
  const type T = int;
  static public int $x = 0;
  public function __construct() {}
}

final class HasTCHasTCInt {
  const type T = HasTCInt;
  public function __construct() {}

  // below is just experiments towards a later test
  public function Tfactory(): HasTCInt {
    $cn = type_structure(static::class, 'T')['classname'];
    $v = new $cn();
    return $v;
  }
}

// for testing the case where we recurse
final class HasTCIntBis {
  const type T = HasTCInt::T;
  public function __construct() {}
}

class TCTester {

  private async function fail(): Awaitable<int> {
    return 42;
  }

  public async function typeTestAgainstTCOK(): Awaitable<void> {
    if (3 is HasTCInt::T) {
      return;
    }
    $_ = $this->fail();
  }

  public async function typeTestAgainstTCBisOK(): Awaitable<void> {
    if (3 is HasTCIntBis::T) {
      return;
    }
    $_ = $this->fail();
  }

  public async function typeTestAgainstTCBad(): Awaitable<void> {
    if (3 is HasTCHasTCInt::T) {
      return;
    }
    $_ = $this->fail();
  }

  public async function typeTestAgainstTC2Bad(): Awaitable<void> {
    $x = new HasTCInt();
    if ($x is HasTCInt::T) { // HasTCInt is not int
      return;
    }
    $_ = $this->fail();
  }

  public async function typeTestAgainstTC2BisBad(): Awaitable<void> {
    $x = new HasTCInt();
    if ($x is HasTCIntBis::T) { // HasTCInt is still not int
      return;
    }
    $_ = $this->fail();
  }

  public async function typeTestAgainstTC2OK(): Awaitable<void> {
    $x = new HasTCInt();
    if ($x is HasTCHasTCInt::T) { // HasTCHasTCInt::T = HasTCInt
      return;
    }
    $_ = $this->fail();
  }
  // there's another natural version of the above test that does HasTCHasTCInt::T::T but it
  // would fall foul of our unsatisfactory modelling of vecs, so I'm leaving it out for now

  public function bar(): void {
    $v = HasTCInt::$x;
    HasTCInt::$x = $v + 1;
  }

}
