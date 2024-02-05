// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace TypeTestTest;

class Wrapper {
  function primitiveTag(mixed $v): int {
    if ($v is null) {
      return 0;
    }
    if ($v is int) {
      return 1;
    }
    if ($v is bool) {
      return 2;
    }
    if ($v is float) {
      return 3;
    }
    if ($v is string) {
      return 4;
    }
    return -1;
  }

  async function fail(): Awaitable<int> {
    return 99;
  }

  async function checkNullOK(): Awaitable<void> {
    if (null is null) {
      return;
    }
    $this->fail();
  }

  async function checkIntOK(): Awaitable<void> {
    if (3 is int) {
      return;
    }
    $this->fail();
  }

  async function checkBoolOK(): Awaitable<void> {
    if (true is bool) {
      return;
    }
    $this->fail();
  }

  async function checkStringOK(): Awaitable<void> {
    if ("foo" is string) {
      return;
    }
    $this->fail();
  }

  async function checkNullBad(): Awaitable<void> {
    if (null is int) {
      return;
    }
    $this->fail();
  }

  async function checkIntBad(): Awaitable<void> {
    if (3 is bool) {
      return;
    }
    $this->fail();
  }

  async function checkBoolBad(): Awaitable<void> {
    if (true is string) {
      return;
    }
    $this->fail();
  }

  // here we use specialised isnull check
  async function checkStringBad(): Awaitable<void> {
    if ("foo" is null) {
      return;
    }
    $this->fail();
  }

}
