// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class NewDictTest {
  public static async function genInt(): Awaitable<int> {
    return 42;
  }

  public static async function makeAndAwaitOK(string $s): Awaitable<void> {
    $a = genInt();
    $d = dict[];
    $d[$s] = $a;
    await Dict\from_async($d);
  }

  public static async function makeAndDontAwaitBadFN(
    string $s,
  ): Awaitable<void> {
    $a = genInt();
    $d = dict[];
    $d[$s] = $a;
  }

  public static async function makeAndAwaitConstantOK(): Awaitable<void> {
    $a = genInt();
    $d = dict[];
    $d["foo"] = $a;
    await Dict\from_async($d);
  }

  public static async function makeAndAwaitEqualityOK(
    string $s,
  ): Awaitable<void> {
    $d = dict[];
    if ($s == 'foo') {
      $a = genInt();
      $d[$s] = $a;
    }
    await Dict\from_async($d);
  }

  public static async function sanityCheckOK(): Awaitable<void> {
    $a = genInt();
    $d = dict['foo' => $a];
    await Dict\from_async($d);
  }

  public static async function sanityCheckBad(): Awaitable<void> {
    $a = genInt();
    $d = dict['foo' => $a];
  }
}
