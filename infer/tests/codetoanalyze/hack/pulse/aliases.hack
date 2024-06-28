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

type TShape = shape();

// should come out as HackArray
type AShapeAlias = shape('success' => bool, 'count' => int);
// should come out as HackDict
type ADictAlias = dict<string, mixed>;
// should come out as HackMixed
type AFunctionAlias = (function(int): int);

// TODO: this currently comes out as void, would rather just use null/HackNull
// but at least the parser doesn't fall over any more
type ANullAlias = null;

class HasConstant {
  const type T = int;
}

final class C {

  public function returnAliasedShape(): TShape {
    return shape();
  }

  // want to make sure that return type enforcement doesn't yield
  // no disjuncts for returnAliasedShape
  public async function testAliasedShapeReturnBad(): Awaitable<void> {
    $s = $this->returnAliasedShape();
    $_ = $this->fail();
  }

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

  public async function testGenericAliasOK(): Awaitable<void> {
    $x = new D<int>();
    if ($x is AliasD<_>) {
      return;
    }
    $_ = $this->fail();
  }

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
