// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace TypeTestTest;

final class C {}

class Wrapper {
  public async function fail(): Awaitable<int> {
    return 99;
  }

  public async function checkNullOK(): Awaitable<void> {
    if (null is null) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkIntOK(): Awaitable<void> {
    if (3 is int) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkcheckIntIsBoolOK(): Awaitable<void> {
    $b = 3 is int;
    if ($b is bool) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkcheckIntIsBoolBad(): Awaitable<void> {
    $b = "foo" is int;
    if ($b is bool) {
      $_ = $this->fail();
    }
    return;
  }

  public async function checkBoolOK(): Awaitable<void> {
    if (true is bool) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkStringOK(): Awaitable<void> {
    if ("foo" is string) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkNullBad(): Awaitable<void> {
    if (null is int) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkIntBad(): Awaitable<void> {
    if (3 is bool) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkBoolBad(): Awaitable<void> {
    if (true is string) {
      return;
    }
    $_ = $this->fail();
  }

  // here we use specialised isnull check
  public async function checkStringBad(): Awaitable<void> {
    if ("foo" is null) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkClassOK(): Awaitable<void> {
    $c = new C();
    if ($c is C) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkClass2OK(): Awaitable<void> {
    if (3 is C) {
      $_ = $this->fail();
    }
    return;
  }

  public async function checkClass2Bad(): Awaitable<void> {
    if (3 is C) {
      return;
    }
    $_ = $this->fail();
  }
}
