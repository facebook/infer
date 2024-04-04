// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

final class WrappersTest {
  public static async function genInt(): Awaitable<int> {
    return 42;
  }

  public static async function genWithDefaultStatic(
    int $x = 0,
  ): Awaitable<void> {
    await WrappersTest::genInt();
    return;
  }

  public async function genWithDefault(int $x = 0): Awaitable<void> {
    await WrappersTest::genInt();
    return;
  }

  // all of these had an FP caused by wrappers leading to nested awaitables
  // fixed by identifying wrappers and not treating their tail calls as async
  public async function f1(): Awaitable<void> {
    await self::genWithDefaultStatic();
  }

  public async function f2(): Awaitable<void> {
    await self::genWithDefaultStatic(99);
  }

  public async function f3(): Awaitable<void> {
    await $this->genWithDefault();
  }

  public async function f4(): Awaitable<void> {
    await $this->genWithDefault(666);
  }
}
