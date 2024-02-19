// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Primitives;

class SensitiveClass {}

function condLog(mixed $data, bool $should_log = false): void {
  if ($should_log) {
    \Level1\taintSink($data);
  }
}

function logEnabledExplicitBad(SensitiveClass $sc): void {
  condLog($sc, true);
}

function logDisabledExplicitOk(SensitiveClass $sc): void {
  condLog($sc, false);
}

function logDisabledImplicitOk(SensitiveClass $sc): void {
  condLog($sc);
}
