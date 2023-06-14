// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Closures;

class Delayed {
  public function startAndWait((function(): Awaitable<void>) $action) : void {
    \HH\Asio\join($action());
  }
}

class Utils {
  public function logDelayed(mixed $data) : void {
    new Delayed()->startAndWait(async () ==> {
      \Level1\taintSink($data);
    });
  }
}

class C1 {
  public function f1Bad(SensitiveClass $sc) : void {
    new Utils()->logDelayed($sc);
  }
}
