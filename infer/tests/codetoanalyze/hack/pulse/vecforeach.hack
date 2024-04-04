// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

async function genInt5(): Awaitable<int> {
  return 42;
}

async function vecForeachOK(): Awaitable<void> {
  $v = vec[genInt5(), genInt5(), genInt5()];
  foreach ($v as $elt) {
    await $elt;
  }
}

async function vecForeachOK2(): Awaitable<void> {
  $v = vec[genInt5(), genInt5(), genInt5()];
  foreach ($v as $key => $elt) {
    await $elt;
  }
}

async function vecForeachBad(): Awaitable<void> {
  $v = vec[genInt5(), genInt5(), genInt5()];
  foreach ($v as $key => $elt) {
    if ($key < 1) {
      await $elt;
    }
  }
}

async function vecForeachBad2(): Awaitable<void> {
  $v = vec[genInt5()];
  foreach ($v as $key => $elt) {
    if (2 < $key) {
      await $elt;
    }
  }
}
