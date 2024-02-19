// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace SelfTest;

class Baz {
  private Awaitable<int> $field;
  private dict<string, mixed> $fields = dict[];

  public function __construct() {
    $this->field = async {
      return 42;
    };
  }

  public async function fooOK(): Awaitable<void> {
    $this->field = async {
      return 42;
    };
    $this->fields["foo"] = async {
      return 43;
    };
  }

  public async function fooBad(): Awaitable<void> {
    $_ = async {
      return 42;
    };
  }

  // The following is useful for tracing failures of the above
  public function testdict(): void {
    $d = dict[];
    $d["foo"] = 42;
  }

  public function testdict2(): void {
    $this->fields["foo"] = 42;
  }
}
