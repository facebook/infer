// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

async function genInt2(): Awaitable<int> {
  return 42;
}

// this should trigger an alert
async function constVecBad(): Awaitable<void> {
  $v = vec[genInt2(), genInt2(), genInt2()];
}

// with only 2 elements this one should be ok without implicitly awaiting spills
async function constVecOk(): Awaitable<void> {
  $v = vec[genInt2(), genInt2()];
  await $v[0];
  await $v[1];
}

// this is ok, expect to see it as ok
async function constVecOk2(): Awaitable<void> {
  $v = vec[genInt2(), genInt2(), genInt2()];
  await $v[0];
  await $v[1];
  await $v[2];
}

//this is actually bad, but expect false negative with current implementation
async function constVecBad2FN(): Awaitable<void> {
  $v = vec[genInt2(), genInt2(), genInt2()];
  await $v[0];
  await $v[2];
}

async function loopyVecOk(): Awaitable<void> {
  $v = vec[genInt2(), genInt2(), genInt2()];
  foreach ($v as $index => $elt) {
    await $elt;
  }
}

async function loopyVecBad(): Awaitable<void> {
  $v = vec[genInt2(), genInt2(), genInt2()];
  foreach ($v as $index => $elt) {
    if ($index === 2) {
      await $elt;
    }
  }
}
