// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class C {}

async function genC () : Awaitable<C> {
  return new C();
}

async function genInt () : Awaitable<int> {
  return 42;
}

async function genGood () : Awaitable<void> {
  await genInt();
}

async function genBad () : Awaitable<void> {
  $x = genInt();
}

function returnInt () : int {
  return 42;
}

function callgood () : void {
  returnInt();
}

// Can't do this yet because front end doesn't actually deal with await properly
// async function genAwaitParam(Awaitable<int> $a) : Awaitable<void> {
//   await($a);
//   return;
// }

// async function genFoo() : Awaitable<void> {
//   $x = genInt();
//   genAwaitParam($x);
//   return;
// }
