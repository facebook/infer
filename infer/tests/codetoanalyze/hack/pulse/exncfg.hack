// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// this corresponds to exncfg.sil in the sil tests directory
// except that has calls to hhbc_throw; unreachable replaced
// by Textual's Throw terminator.
// This version is expected show an internal error due to incorrect
// liveness (until we update hackc to emit Throw)
// whereas the sil file should be OK
function main(string $v, bool $b): void {
  if ($b) {
    $_ = $v is string;
    throw new Exception();
  }
}
