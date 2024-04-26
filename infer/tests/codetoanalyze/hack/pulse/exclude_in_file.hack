// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Exclusions;

class Source {
  public static function get(): int {
    return 42;
  }
}

class Sink {
  public static function consume(mixed $arg): void {}
}

class FlowsExcludeInFile {
  public static function excludedFlowOk(): void {
    Sink::consume(Source::get());
  }
}
