// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// UAA examples from Mistral's post
namespace Mistral;

async function gen(): Awaitable<int> {
  return 42;
}

function takes_anything_does_nothing(mixed $_): void {}

async function oops(): Awaitable<void> {
  $i = gen();
  takes_anything_does_nothing($i); // error
}

async function awaits_arg(Awaitable<int> $x): Awaitable<void> {
  await $x;
}

async function notoops(): Awaitable<void> {
  $i = gen();
  $_ = awaits_arg($i); // No error
}

function oops_vec(): void {
  $v = vec[gen(), gen(), gen()]; // error
}

class D {
  public function gen(): mixed {
    return 42;
  }
}

class C extends D {
  <<__Override>>
  public async function gen(): Awaitable<int> {
    return 42;
  }
}

function oops1(bool $b): void {
  $obj = $b ? new D() : new C();
  $obj->gen(); // error
}

function oops2(bool $b): void {
  if ($b) {
    $x = (new C())->gen(); // error
  } else {
    $x = (new D())->gen();
  }
}
