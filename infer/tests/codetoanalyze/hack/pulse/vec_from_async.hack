// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class C {
  public static function foo(): int {
    return 3;
  }
}

function blah(): int {
  return C::foo();
}

async function genInt4(): Awaitable<int> {
  return 42;
}

// this should trigger an alert
async function vecFromAsyncBad(): Awaitable<void> {
  $v = vec[genInt4(), genInt4(), genInt4()];
  $t = Vec\from_async($v);
}

async function vecFromAsyncOk(): Awaitable<void> {
  $v = vec[genInt4(), genInt4(), genInt4()];
  $t = Vec\from_async($v);
  await $t;
}

async function vecFromAsyncBad2(): Awaitable<void> {
  $v = vec[genInt4(), genInt4(), genInt4()];
  $t = Vec\from_async($v);
  $v[] = genInt4();
  await $t;
}
