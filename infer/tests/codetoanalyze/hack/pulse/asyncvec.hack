// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

async function genInt3(): Awaitable<int> {
  return 42;
}

// this should trigger an alert
async function vecAppendBad(): Awaitable<void> {
  $v = vec[];
  $v[] = genInt3();
}

async function vecAppendOk(): Awaitable<void> {
  $v = vec[];
  $v[] = genInt3();
  await $v[0];
}

async function vecupdateBad(): Awaitable<void> {
  $v = vec[genInt3(), genInt3()];
  $v[0] = genInt3();
  await $v[1];
}

// this is actually leaky, but we expect false negative
async function vecupdateFN(): Awaitable<void> {
  $v = vec[genInt3(), genInt3()];
  $v[0] = genInt3();
  await $v[1];
  await $v[0];
}

async function nodynamictypeOK_FP(vec<Awaitable<int>> $c): Awaitable<void> {
  $c[0] = genInt3();
  await $c[0];
}

async function vecAccessFP(): Awaitable<void> {
  $v = vec[genInt3(), genInt3()];
  await $v[0];
  $_dummy = $v[0];
  await $v[1];
}
