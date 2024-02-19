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
async function dictForeachOK2(): Awaitable<void> {
  $d = dict[];
  foreach ($d as $v) {
    $_ = genInt7();
  }
}

async function dictForeachBad2(): Awaitable<void> {
  $d = dict['a' => 'b'];
  foreach ($d as $v) {
    $_ = genInt7();
  }
}

async function dictFromAsyncOK(): Awaitable<void> {
  $d = dict['a' => genInt7(), 'b' => genInt7()];
  await Dict\from_async($d);
}

async function dictFromAsyncBad(): Awaitable<void> {
  $d = dict['a' => genInt7(), 'b' => genInt7()];
  $_ = Dict\from_async($d);
}

async function do_await_dict_from_async(
  dict<string, Awaitable<int>> $d,
): Awaitable<void> {
  await Dict\from_async($d);
}

// this demonstrates the need for a better treatment of less-concrete dicts
async function dictFromAsyncIndirectFP(): Awaitable<void> {
  $d = dict['a' => genInt7(), 'b' => genInt7()];
  await do_await_dict_from_async($d);
}
