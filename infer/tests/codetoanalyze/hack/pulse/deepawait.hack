// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class ContainsAwaitable {
  public Awaitable<int> $a;

  public function __construct() {
    $this->a = async {
      return 42;
    };
  }

}

// this should be OK
async function deepTestOK(): Awaitable<void> {
  $v = vec[
    new ContainsAwaitable(),
    new ContainsAwaitable(),
    new ContainsAwaitable(),
  ];
  foreach ($v as $x) {
    await $x->a;
  }
}

// this should also be OK (but only because we know we're dropping one)
async function deepTestFNBad(): Awaitable<void> {
  $v = vec[
    new ContainsAwaitable(),
    new ContainsAwaitable(),
    new ContainsAwaitable(),
  ];
  await $v[0]->a;
  await $v[1]->a;
}

// this should raise an error, obviously
async function deepTestBad(): Awaitable<void> {
  $v = vec[
    new ContainsAwaitable(),
    new ContainsAwaitable(),
    new ContainsAwaitable(),
  ];
  await $v[0]->a;
}
