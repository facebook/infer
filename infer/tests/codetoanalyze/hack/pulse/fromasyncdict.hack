// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class MyExn2 extends Exception {}

class SuppressExceptional {
  public static async function genFirstThing(): Awaitable<string> {
    return "foo";
  }

  public static async function genSecondThing(): Awaitable<string> {
    throw new MyExn2();
    return "bar";
  }

  public static async function genTestOK(): Awaitable<void> {
    $a = self::genFirstThing();
    $b = self::genSecondThing();
    await $a;
    await $b;
  }
}

class CowSetKnown {
  public static async function genFirstThing(): Awaitable<string> {
    return "foo";
  }

  public static async function genSecondThing(): Awaitable<string> {
    throw new MyExn2();
    return "bar";
  }

  public static async function genTestOK(): Awaitable<void> {
    $d = dict[];
    $d['first'] = self::genFirstThing();
    $d['second'] = self::genSecondThing();
    // leaving the await commented out is OK because
    // we know we'll throw and suppress the error
    // await Dict\from_async($d);
  }
}

class FromARealDiff {

  public static async function genFirstThing(): Awaitable<string> {
    return "foo";
  }

  public static async function genSecondThing(): Awaitable<string> {
    throw new MyExn2();
    return "bar";
  }

  public static async function genThirdThing(): Awaitable<string> {
    return "baz";
  }

  public static async function genTranslated(
    vec<string> $keys,
  ): Awaitable<dict<string, string>> {
    $translated_things = dict[];

    foreach ($keys as $key) {
      switch ($key) {
        case 'first':
          $translated_things[$key] = self::genFirstThing();
          break;
        case 'second':
          $translated_things[$key] = self::genSecondThing();
          break;
        case 'third':
          $translated_things[$key] = self::genThirdThing();
          break;
        default:
          break;
      }
    }

    return await Dict\from_async($translated_things);
  }

  public static async function genTest(): Awaitable<void> {
    await self::genTranslated(vec['first', 'second']);
  }
}
