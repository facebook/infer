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

  // this is designed to test preservation of instanceof facts
  // works fine because we do add_term_eq and then merge_vars
  // does the right thing
  public async function preserveTypeTestOK(mixed $m): Awaitable<void> {
    if ($m is D) {
      if ($m is D) {
        return;
      } else {
        $_ = $this->fail();
      }
    } else {
      return;
    }
  }

  // C and E are incomparable
  public async function belowFactPreservedOK(mixed $m): Awaitable<void> {
    if ($m is C) {
      if ($m is E) {
        $_ = $this->fail();
        // can't happen because $m can't be C and E at the same time
        // note that that generall depends on the assumption that types form a tree
        // rather than a DAG. If C & D were interfaces it would be wrong
      }
    }
    return;
  }

  // D is a subtype of E
  public async function belowFactImplicationBad(mixed $m): Awaitable<void> {
    if ($m is E) {
      if ($m is D) {
        $_ = $this->fail();
        // always fails
      }
    }
    return;
  }

  public async function belowFactImplicationOK(mixed $m): Awaitable<void> {
    if ($m is E) {
      if ($m is D) { // always succeeds
        return;
      } else {
        $_ = $this->fail(); // never happens
      }
    }
    return;
  }
}
