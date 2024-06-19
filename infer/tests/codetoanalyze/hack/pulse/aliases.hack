// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.
<<file: __EnableUnstableFeatures('case_types')>>

namespace Aliases;

type A = int;
newtype B = A;

class D<T> {}
type AliasD<T> = D<T>;

// the following is just here to test that the alias parsing doesn't fall over too badly
// should it encounter a case type - we don't actually do anything sensible with them yet
case type CT3 = string | bool;

class HasConstant {
  const type T = int;
}

class C {
  public function foo(A $x): int {
    return $x;
  }

  public async function fail(): Awaitable<int> {
    return 42;
  }

  public async function testGenericOK(): Awaitable<void> {
    $x = new D<int>();
    if ($x is D<_>) {
      return;
    }
    $_ = $this->fail();
  }

  /* I think this ought to work, but hh rejects it for some reason!
     Bug reported to Hack team
     Fixed in D58323800
    public async function testGenericAliasOK(): Awaitable<void> {
      $x = new D<int>();
      if ($x is AliasD<_>) {
        return;
      }
      $_ = $this->fail();
    }
    $_ = $this->fail();
  }
  */

  // This one doesn't work yet because we're not doing explicit parameter assertion/verification
  // on non-generic parameters yet (Hackc does them implicitly now)
  public async function testParameterAliasOK_FP(A $x): Awaitable<void> {
    if ($x is int) {
      return;
    }
    $_ = $this->fail();
  }

  // so this one, which seems more complex, works OK 'cos Hackc does emit explicit
  // parameter verification here
  public async function testParameterGenericAliasOK(
    AliasD<int> $x,
  ): Awaitable<void> {
    if ($x is D<_>) {
      return;
    }
    $_ = $this->fail();
  }

  public async function testPrimitiveAliasOK(): Awaitable<void> {
    if (3 is A) {
      return;
    }
    $_ = $this->fail();
  }

  public async function testPrimitiveAlias2OK(): Awaitable<void> {
    if (3 is B) {
      return;
    }
    $_ = $this->fail();
  }

  // don't do type constants yet :-(
  public async function testTypeConstantOK_FP(): Awaitable<void> {
    if (3 is HasConstant::T) {
      return;
    }
    $_ = $this->fail();
  }
}
