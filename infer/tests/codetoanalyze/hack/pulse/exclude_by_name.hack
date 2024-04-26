// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Exclusions;

class SinkWithExcludedFunction {
  public static function consume(mixed $arg): void {}

  public static function excluded(mixed $arg): void {}
}

class FlowsExcludeByName {
  public static function excludedFlowOk(): void {
    SinkWithExcludedFunction::excluded(Source::get());
  }

  public static function notExcludedFlowBad(): void {
    SinkWithExcludedFunction::consume(Source::get());
  }
}
