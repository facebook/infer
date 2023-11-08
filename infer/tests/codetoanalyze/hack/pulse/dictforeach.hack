// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

async function genInt7(): Awaitable<int> {
  return 42;
}

async function dictForeachOK(): Awaitable<void> {
  $d = dict['a' => genInt7(), 'b' => genInt7()];
  foreach ($d as $k => $elt) {
    await $elt;
  }
}

// short enough for unfoldings
async function dictForeachBad(): Awaitable<void> {
  $d = dict['a' => genInt7(), 'b' => genInt7(), 'c' => genInt7()];
  foreach ($d as $k => $elt) {
    if ($k === 'a') {
      await $elt;
    }
  }
}

// too many unfoldings required for termination
async function dictForeachFN(): Awaitable<void> {
  $d = dict[
    'a' => genInt7(),
    'b' => genInt7(),
    'c' => genInt7(),
    'd' => genInt7(),
    'e' => genInt7(),
  ];
  foreach ($d as $k => $elt) {
    if ($k === 'b') {
      await $elt;
    }
  }
}

// check we know iteration over empty dict does nothing
async function dictforeachOK2(): Awaitable<void> {
  $d = dict[];
  foreach ($d as $v) {
    genInt7();
  }
}

async function dictforeachBad2(): Awaitable<void> {
  $d = dict['a' => 'b'];
  foreach ($d as $v) {
    genInt7();
  }
}
