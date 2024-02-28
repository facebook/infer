// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace GenRunTest;

interface IRunner {
  public function genRun((function(): Awaitable<int>) $fn): Awaitable<void>;
}

final class GoodRunner implements IRunner {
  public async function genRun(
    (function(): Awaitable<int>) $fn,
  ): Awaitable<void> {
    await $fn();
  }
}

final class BadRunner implements IRunner {
  public async function genRun(
    (function(): Awaitable<int>) $fn,
  ): Awaitable<void> {
    return;
  }
}

class GenRunTest {
  public static async function genInt(): Awaitable<int> {
    return 42;
  }

  // this is kind of latent, though?
  public static async function inGeneralCaseBad(
    IRunner $r,
    bool $b,
  ): Awaitable<void> {
    $foo = self::genInt();
    if ($b) {
      await $r->genRun(async () ==> await $foo);
    } else {
      await $foo;
    }
  }

  /* TODO: Fix the issue overwriting issue in DB */
  // this should be bad, needs specialisation to tell, though
  // and that seems rather fragile - whether or not I get an error
  // showing up on inGeneralCaseBad depends on what calls are around
  /* public static async function mainShouldBeBadFN(): Awaitable<void> {
    await self::inGeneralCaseBad(new BadRunner(), true);
  } */

  // neither of these calls are bad, need appropriate specialisation to detect that
  /* public static async function mainShouldBeOK(): Awaitable<void> {
    await self::inGeneralCaseBad(new GoodRunner(), true);
    await self::inGeneralCaseBad(new BadRunner(), false);
  } */
}

// Check we've fixed an FP from a common pattern from www: conditional awaiting
class MaybeContainsAwaitable {
  public function __construct(private mixed $x) {}

  public async function getX(): Awaitable<mixed> {
    if ($this->x is Awaitable<_>) {
      return await $this->x;
    }
    return $this->x;
  }

  public static async function testGetXOK(): Awaitable<void> {
    $a = GenRunTest::genInt();
    $b = new MaybeContainsAwaitable($a);
    $x = await $b->getX();
  }

  // this now works
  public static async function boolTestOK(): Awaitable<void> {
    $b = true;
    if ($b is bool) {
      $a = 10;
    } else {
      $a = GenRunTest::genInt();
    }
  }

  // but some class tests should now be OK
  public static async function classTestOK(): Awaitable<void> {
    $b = new MaybeContainsAwaitable(true);
    if ($b is MaybeContainsAwaitable) {
      $a = 10;
    } else {
      $a = GenRunTest::genInt();
    }
  }

}
