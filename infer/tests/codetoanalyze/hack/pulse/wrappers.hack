// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

final class WrappersTest {
  static async function genInt(): Awaitable<int> {
    return 42;
  }

  static async function genWithDefault(int $x = 0): Awaitable<void> {
    await WrapperTest::genInt();
    return;
  }

  // all of these had an FP caused by wrappers leading to nested awaitables
  // fixed by identifying wrappers and not treating their tail calls as async
  async function f1(): Awaitable<void> {
    await self::genWithDefault();
  }

  async function f2(): Awaitable<void> {
    await self::genWithDefault(99);
  }

  async function f3(): Awaitable<void> {
    await $this->genWithDefault();
  }

  async function f4(): Awaitable<void> {
    await $this->genWithDefault(666);
  }
}
