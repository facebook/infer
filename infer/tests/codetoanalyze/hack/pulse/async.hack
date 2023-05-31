// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

async function genInt () : Awaitable<int> {
  return 42;
}

// this should be picked up as async
async function an_async_not_starting_with_gen () : Awaitable<int> {
  return 42;
}

// this is not async even though the name starts with "gen"
function generate_a_boolean() : bool {
  return true;
}

async function genOk () : Awaitable<void> {
  await genInt();
}

async function genOK2 () : Awaitable<void> {
  $x = genInt();
  await $x;
}

async function genBad () : Awaitable<void> {
  genInt();
}

async function genBad2 () : Awaitable<void> {
  $x = genInt();
}

function produce_awaitable_int () : Awaitable<int> {
  return genInt();
}

async function genBadIndirect () : Awaitable<void> {
  produce_awaitable_int();
}

async function genOkIndirect() : Awaitable<void> {
  await produce_awaitable_int();
}

async function genAwaitParam(Awaitable<int> $a) : Awaitable<void> {
   await($a);
   return;
 }

async function genDontAwaitParam(Awaitable<int> $a) : Awaitable<void> {
  return;
}

async function genAndAwaitOk() : Awaitable<void> {
   $x = genInt();
   genAwaitParam($x);
   return;
}

async function genAndAwaitBad() : Awaitable<void> {
  $x = genInt();
  genDontAwaitParam($x);
  return;
}
