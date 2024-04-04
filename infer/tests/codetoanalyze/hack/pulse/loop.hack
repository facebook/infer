// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// These are horrid test cases: apart from simpleloopBad, Pulse gives up on all the loops

async function genInt6(): Awaitable<int> {
  return 42;
}

async function simpleloopOK(): Awaitable<void> {
  $v = genInt6();
  $n = 0;
  while ($n < 100) {
    $n++;
  }
  await $v;
}

async function simpleloopOK2(): Awaitable<void> {
  $v = genInt6();
  $n = 0;
  $b = true;
  while ($n < 100) {
    $n++;
    if ($b) {
      await $v;
      $b = false;
    }
  }
}

async function loophorror(): Awaitable<void> {
  $n = 0;
  while ($n < 10) {
    $n++;
  }
  $_ = genInt6();
}
async function simpleloopBad(): Awaitable<void> {
  $v = genInt6();
  $n = 0;
  while ($n < 2) {
    $n++;
  }
  if ($n > 20) {
    await $v;
  }
}

async function simpleloopBadFN2(): Awaitable<void> {
  $v = genInt6();
  $n = 0;
  while ($n < 18) {
    $n++;
  }
  if ($n > 20) {
    await $v;
  }
}

async function simpleloopOK3(): Awaitable<void> {
  $v = genInt6();
  $n = 0;
  while ($n < 100) {
    $n++;
  }
  if ($n > 20) {
    await $v;
  }
}

async function simpleloopFN(): Awaitable<void> {
  $v = genInt6();
  $n = 0;
  while ($n < 100) {
    $n++;
  }
  if ($n < 20) {
    await $v;
  }
}

async function simpleloopOK4(): Awaitable<void> {
  $v = genInt6();
  $n = 0;
  while ($n < 100) {
    $n++;
    if ($n === 42) {
      await $v;
    }
  }
}

async function simpleloopOK5(): Awaitable<void> {
  $v = genInt6();
  $n = 0;
  while ($n < 100) {
    $n++;
    if ($n < 1) {
      await $v;
    }
  }
}
