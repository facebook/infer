// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

async function genInt(): Awaitable<int> {
  return 42;
}

// this should be picked up as async
async function an_async_not_starting_with_gen(): Awaitable<int> {
  return 42;
}

// tests for ugly naming rules, deliberately reference unknown functions
async function testnamingOK(): Awaitable<void> {
  generate_a_boolean(); // should not error
}

async function testnamingBad(): Awaitable<void> {
  genFoo(); // should error
}

async function testnamingBad2(): Awaitable<void> {
  gena(); // should error
}

async function testnamingBad3(): Awaitable<void> {
  an_async_not_starting_with_gen(); // should error because we know decl
}

async function genOk(): Awaitable<void> {
  await genInt();
}

async function genOK2(): Awaitable<void> {
  $x = genInt();
  await $x;
}

async function genBad(): Awaitable<void> {
  genInt();
}

async function genBad2(): Awaitable<void> {
  $x = genInt();
}

function produce_awaitable_int(): Awaitable<int> {
  return genInt();
}

async function genBadIndirect(): Awaitable<void> {
  produce_awaitable_int();
}

async function genOkIndirect(): Awaitable<void> {
  await produce_awaitable_int();
}

async function genAwaitParam(Awaitable<int> $a): Awaitable<void> {
  await ($a);
  return;
}

async function genDontAwaitParam(Awaitable<int> $a): Awaitable<void> {
  return;
}

async function genAndAwaitOk(): Awaitable<void> {
  $x = genInt();
  await genAwaitParam($x);
  return;
}

async function genAndAwaitBad(): Awaitable<void> {
  $x = genInt();
  await genDontAwaitParam($x);
  return;
}
