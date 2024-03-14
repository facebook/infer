// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace SubClassTest;

class D {}
final class C extends D {}
class E extends D {}
class F extends E {}

class Wrapper {
  public async function fail(): Awaitable<int> {
    return 99;
  }

  public async function checkReflOK(): Awaitable<void> {
    $v = new C();
    if ($v is C) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkExtendsOK(): Awaitable<void> {
    $v = new C();
    if ($v is D) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkTransOK(): Awaitable<void> {
    $v = new F();
    if ($v is D) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkTransBad(): Awaitable<void> {
    $v = new F();
    if ($v is D) {
      $_ = $this->fail();
    }
    return;
  }
}
