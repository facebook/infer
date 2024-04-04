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

  // Note that the actual issue is reported at the callee
  // inGeneralCaseBad.
  public static async function callInGeneralCaseBad1(): Awaitable<void> {
    await self::inGeneralCaseBad(new BadRunner(), true);
  }

  public static async function callInGeneralCaseBad2(): Awaitable<void> {
    await self::inGeneralCaseBad(new GoodRunner(), true);
    await self::inGeneralCaseBad(new BadRunner(), false);
  }

  // This function is exactly the same to inGeneralCaseBad above, but
  // it is OK because is invoked only with safe arguments from
  // callIngeneralCaseOK below.
  public static async function inGeneralCaseOK(
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

  public static async function callInGeneralCaseOK(): Awaitable<void> {
    await self::inGeneralCaseOK(new GoodRunner(), true);
    await self::inGeneralCaseOK(new BadRunner(), false);
  }
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
