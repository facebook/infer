// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace AsyncInterface;

interface IAsync {
  public function genFoo(): Awaitable<int>;
}

async function callinterfaceOK(IAsync $a): Awaitable<int> {
  return await $a->genFoo();
}

async function callinterfaceBad(IAsync $a): Awaitable<int> {
  $x = $a->genFoo();
  return 42;
}
